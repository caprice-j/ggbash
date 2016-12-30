#' @import ggplot2
NULL

truncate_strings <- function(colnamev=c('mpg', 'cyl', 'disp', 'hp', 'drat', 'wt'), i=1) {

    nchar_longest <- max(sapply(colnamev, nchar))

    out <- rep('', length(colnamev))
    while (any(out == '')) {
        shortened_colnamev <- sapply(colnamev, function(s) substr(s,1,i))
        dup_namev <- names(table(shortened_colnamev))[table(shortened_colnamev) > 1]
        index <- (! shortened_colnamev %in% dup_namev) & (out == '')
        out[index] <- shortened_colnamev[index]
        i <- i + 1
    }

    short2colname <- list()

    for (i in seq_along(colnamev)) {
        short2colname[ out[i] ] <- colnamev[i]
    }
    return(short2colname)
}

show_dataset_column_indices <- function(dataset=NULL){
    if (is.null(dataset))
        return()
    nchar_longest <- max(sapply(colnames(dataset), nchar))
    short_colnamel <- truncate_strings(colnames(dataset), i=4)
    # i = 4 because too short colnames are hard to read
    mod <- 5
    linev <- rep('', mod)
    for ( i in seq_along(short_colnamel) ) {
        this <- names(short_colnamel)[i]
        index <- ((i-1) %% mod) + 1
        linev[index] <- paste0(linev[index],
                               stringr::str_pad(i,
                                       width=nchar(ncol(dataset))), ': ',
                               stringr::str_pad(this,
                                       width=nchar_longest,
                                       side='right'), '\t')
    }

    for (i in 1:mod ){
        if (linev[i]!='')
            message(linev[i])
    }
}

show_prompt <- function(dataset=NULL){
    ds_str <- attr(dataset, 'ggbash_datasetname')
    username <- Sys.info()['user']
    hostname <- Sys.info()['nodename']
    working_dir <- basename(getwd())
    ds_str <- ifelse(is.null(ds_str),'', paste0(' (', ds_str,')'))
    ggbash_prompt <- paste0(username, '@',
                            hostname, ' ',
                            working_dir, ds_str, ' $')
    return(readline(prompt=ggbash_prompt))
}

splib_by_pipe <- function(input='point x=3 y=4 color=5 | copy'){
    return(stringr::str_split(input, '\\|')[[1]])
}

split_by_space <- function(input='    point x=3 y=4 color=5 '){
    # remove preceding/trailing spaces
    argv <- stringr::str_split(input, ' ')[[1]]
    return(argv[nchar(argv)>0])
}

add_input_to_history <- function(input='point 2 3'){
    history_file <- tempfile("Rhistoryfile")
    savehistory(history_file)

    cat(input, '\n', file=history_file, append = TRUE)
    loadhistory(history_file)
    unlink(history_file)
}

execute_builtins <- function(raw_input, argv, const, dataset){
    if (argv[1] %in% c('pwd', 'getwd')) {
        message(getwd())
    } else if (argv[1] %in% c('ls')) {
        show_dataset_column_indices(dataset)
    } else if (argv[1] %in% c('dir')) {
        message( paste(dir(getwd()), collapse='\t') )
        # TODO ls -l
    } else if (argv[1] %in% c('cd', 'setwd')) {
        if (length(argv)<2)
            setwd(const$first_wd)
        else
            setwd(argv[2])
    } else if (argv[1] %in% c('echo', 'print')) {
        message(raw_input)
    }
}

# load_libraries <- function(){
#     lib <- 'stringr' # for test
#     for (lib in c('stringr', 'dplyr', 'ggplot2')) {
#         if (!suppressWarnings(require(lib, character.only=TRUE)))
#             stop('You need to install library(', lib, ') to execute ggbash.')
#     }
# }

define_constant_list <- function(){
    list(
        first_wd = getwd(),
        # BUILTIN command Vectors
        builtinv = c('cd', 'echo', 'exit', 'ls', 'pwd', 'quit'),
        # all geom in ggplot2 documents
        # the order in geom_namev is important
        # because build_ggplot_object() uses the first element after partial matching
        # i.e. the preferable (frequently-used) geom should appear first
        geom_namev = c('abline', 'area',
                       'bar', 'bin2d', 'blank', 'boxplot',
                       'count', 'curve', 'contour', 'crossbar',
                       'density', 'density_2d', 'dotplot',
                       'errorbar', 'errorbarh',
                       'freqpoly',
                       'hline','hex',
                       'jitter',
                       # 'l' matches to 'line' (the first element starting by 'l')
                       'line', 'label', 'linerange',
                       'map',
                       # 'p' matches to 'point'
                       'point', 'path', 'polygon', 'pointrange',
                       'quantile',
                       'rug', 'raster', 'ribbon',
                       'segment', 'smooth',
                       'vline', 'violin'
        )
        # TODO implement stat like stat_smooth
    )
}

set_dataset <- function(argv){
    dataset <- dplyr::tbl_df(eval(as.symbol(((argv[2])))))
    message('attach ', argv[2])
    dplyr::glimpse(dataset)
    attr(dataset, 'ggbash_datasetname') <- argv[2]
    # should I store the var name with parentheses?
    return(dataset)
}

copy_to_clipboard <- function(string){
    os <- Sys.info()['sysname']
    if (os == 'Darwin') {
        cat(string, file=(con <- pipe('pbcopy', 'w')))
        message('copied to clipboard:\n', string)
        close(con)
    } else {
        stop('copy in Windows / Linux is to be implemented')
    }
}

save_ggplot <- function(ggplot_command, argv=c('save', 'big') ){
    # TODO
    #confl <- build_conf(argv)
    #png(filename=, height = confl$h, confl$w)
}

#' Enter into a ggbash session.
#'
#' \code{ggbash} executes a new ggbash session for faster ggplot2 plotting.
#'
#' ggbash provides concise aliases for ggplot2 functions.
#' By calling ggbash(), your R session goes into a ggbash session,
#' which only interprets predefined ggbash commands.
#' Some basic commands like setwd() or pwd() works in ggbash session,
#' but most of the usual R grammars are disabled.
#' Instead, a variety of ggbash commands are enabled
#' for writing ggplot2 script as faster as possible.
#'
#' @param dataset a dataframe to attach. Default is NULL.
#'                You can define the dataframe later
#'                by 'use your_dataset' command in your ggbash session.
#'                If a matrix object is given, It's automatically
#'                converted into a tbl_df object.
#' @param ambiguous_match A boolean whether to do ambiguous match when a ggbash command ambiguously matches several commands. Default is TRUE. The matching rules are as follows:
#' \describe{
#'     \item{Geom name:}{the geom most frequently used (based on my experiences)}
#'     \item{Column name:}{the column with the smallest column index}
#'     \item{Aesthetics:}{required (x, y), non-missing (shape, size), default (alpha, stroke) }
#' }
#' @return nothing
#' @examples
#' ggbash()
#' ggbash(iris)
#' @export
ggbash <- function(dataset = NULL, ambiguous_match=TRUE) {

    # initialization
    if (! is.null(dataset))
        attr(dataset, 'ggbash_datasetname') <- deparse(substitute(dataset))
    load_libraries()
    const <- define_constant_list()
    exit <- FALSE

    while (TRUE) { tryCatch(
        {   # main loop for command execution

            raw_input <- show_prompt(dataset)
            commandv <- splib_by_pipe(raw_input)
            print('commandv ')
            print(commandv)
            for (cmd in commandv) {
                argv <- split_by_space(cmd)
                print('argv: ')
                print(argv)
                if (argv[1] %in% c('exit', 'quit')) {
                    exit <- TRUE
                    break
                } else if (argv[1] == 'use') {
                    dataset <- set_dataset(argv)
                } else if (argv[1] == 'show') {
                    print(dplyr::tbl_df(eval(as.symbol((argv[2])))))
                } else if (argv[1] %in% const$builtinv) {
                    execute_builtins(raw_input, argv, const, dataset)
                } else if (argv[1] %in% c('copy', 'cp')) {
                    copy_to_clipboard(executed_command)
                } else if (argv[1] %in% const$savev) {
                    save_ggplot(executed_command, argv)
                } else { # if 'point' or 'p' is passed
                    executed_command <- build_ggplot_object(argv,
                                                            dataset,
                                                            const)
                }

            }
            if (exit)
                break # FIXME an ugly way to avoid returning NULL
        },
        warning = function(wrn) {
                                    message('I got warning', wrn)
                                },
          error = function(err) { # stop() goes here
                                    message('ERROR: ', err)
                                },
        finally = {
                    add_input_to_history(raw_input) # add to history even if failed
        }
    ) }
}

get_required_aes <- function(suffix='point') {
    command <- paste0('geom_', suffix, '()')
    expr <- parse(text = command)
    return(eval(expr)$geom$required_aes)
}

get_possible_aes <- function(suffix='point') {
    command <- paste0('geom_', suffix, '()')
    expr <- parse(text = command)
    geom <- eval(expr)$geom
    possible_aesv <- unique(c(geom$required_aes,
                              geom$non_missing_aes,
                              names(geom$default_aes)))
    return(truncate_strings(possible_aesv))
}

find_index <- function(pattern='siz', stringv=c('x', 'y', 'si', 'sh')){
    return(! c(is.na(stringr::str_match(string=stringv, pattern=paste0('^',pattern)))))
}

build_ggplot_object <- function(argv=c('p','x=2','y=3','color=4','size=5'), dataset, const){

    if (is.null(dataset))
        stop('dataset is not set')

    # p -> 'point'
    geom_sth <- const$geom_namev[ find_index(pattern=argv[1], stringv=const$geom_namev) ][1]
    message('selected geom: ', geom_sth)

    required_aesv <- get_required_aes(geom_sth)
    all_aesl <- get_possible_aes(geom_sth)
    message('all_aesl: ', paste0(all_aesl, collapse=' '))

    colnamev <- colnames(dataset)


    short2colname <- truncate_strings(colnamev)
    # if all required aesthetics are set
    #
    # TODO set non-aes elements
    conf <- list(aes=list())
    i <- 2
    aesv <- argv[-1]
    for ( i in seq_along(aesv) ) {
        # TODO as.factor
        # TODO as.character
        # TODO cut
        # TODO substr
        if (grepl('=', aesv[i])) {
            before_equal <- gsub('=.*', '', aesv[i])
            after_equal  <- gsub('.*=',     '', aesv[i])
        } else { # no aes specification like geom_point(aes(my_x, my_y))
            before_equal <- required_aesv[i]
            after_equal  <- aesv[i]

            if (i > length(required_aesv))
                stop('too many unspecified aesthetics. ',
                     'Required aesthetics (in order) are: ',
                     paste0(required_aesv, collapse=', '))
        }

        if (! before_equal %in% all_aesl) {
            index = find_index(pattern=before_equal, stringv=unlist(all_aesl))
            selected <- unlist(all_aesl)[index][1]
            if (sum(index)>1)
                message('[ !!! CAUTION !!! ] ambiguous aes. use "', selected,
                        '" among ', paste0(unlist(all_aesl)[index], collapse=', '))
            before_equal <- selected # FIXME refactor
        }

        if (grepl('[0-9]', after_equal))
            after_equal <- colnamev[as.numeric(after_equal)]
        else if (! after_equal %in% colnamev) {
            index = find_index(pattern=after_equal, stringv=colnamev)
            selected = unlist(short2colname)[index][1]
            if (sum(index)>1)
                message('[ !!! CAUTION !!! ] ambiguous aes. use "', selected,
                        '" among ', paste0(unlist(short2colname)[index], collapse=', '))

            after_equal <- selected
        }

        conf$aes[[i]] <- paste0(before_equal, '=', after_equal)
    }
    command <- paste0('ggplot(',attr(dataset, 'ggbash_datasetname'),')',
                      ' + geom_', geom_sth, '(',
                      'aes(', paste0(conf$aes, collapse = ', '), '))')
    ncmd <- nchar(command) # it's unfair to include labs() characters.
    #command <- paste0(command, ' + labs(subtitle="', command, '")')
    expr <- parse(text = command)
    print(eval(expr))
    message('executed (', ncmd, ' characters) :\n ', command)
    return(command)
}
