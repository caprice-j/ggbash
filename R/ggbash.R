#' @import ggplot2
NULL

#' Construct partially unique string list
#'
#' \code{partial_unique} makes given characters as short as posssible while preserving uniqueness.
#'
#' \code{partial_unique} is a variant of R's builtin \code{\link[base]{unique}} function.
#' While \code{link[base]{unique}} returns the non-duplicated set of elements,
#' \code{partial_unique} returns a list whose element names are partially unique shortest strings,
#' and its element values are original strings.
#' This function can display how many characters
#' are needed to uniquely identify each given character element.
#'
#' @param originalv a character vector to construct partial strings.
#' @param i The smallest character of the resulted partial strings.
#'          Sometimes too small i loses readability
#'          such as in \code{\link{show_dataset_column_indices}}
#'
#' @return a list whose element names are partial strings and element values are original strings.
#'
#' @examples
#' partial_unique(c("Sepal.Width", "Sepal.Length", "Species", "Petal.Width"))
#' # returns list(Sepal.W = "Sepal.Width", Sepal.L = "Sepal.Length",
#' #              Sp = "Species", P = "Petal.Width")
#'
#' @seealso \code{\link[base]{unique}}, \code{\link{show_dataset_column_indices}}
#'
#' @export
partial_unique <- function(originalv=c('mpg', 'cyl', 'disp', 'hp', 'drat'), i=1) {

    nchar_longest <- max(sapply(originalv, nchar))

    out <- rep('', length(originalv))
    while (any(out == '')) {
        shortened_colnamev <- sapply(originalv, function(s) substr(s,1,i))
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

find_first <- function(prefix='si', table=c('x', 'y', 'size', 'shape')){
    indices <- grep(paste0('^', prefix), table)
    if (length(indices)<1)
        stop('no such prefix')
    if (length(indices)>1)
        message('[ !!! CAUTION !!! ] ambiguous match.',
                ' use "', table[indices][1],
                '" among ', paste0(table[indices], collapse=', '))
    return(indices[1])
}

show_dataset_column_indices <- function(dataset=NULL){
    if (is.null(dataset))
        return()
    nchar_longest <- max(sapply(colnames(dataset), nchar))
    short_colnamel <- partial_unique(colnames(dataset), i=4)
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
                            working_dir, ds_str, ' $ ')
    return(readline(prompt=ggbash_prompt))
}

splib_by_pipe <- function(input='point x=3 y=4 color=5 | copy'){
    return(stringr::str_split(input, '\\|')[[1]])
}

#' split a given string by spaces
#'
#' @param input A character. Typically one of the elements returned by \code{\link{split_by_pipe}}.
#' @return A character vector
#'
#' @export
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
    } else if (argv[1] %in% c('mkdir', 'dir.create')) {
        dir.create(argv[2], recursive=TRUE)
    } else if (argv[1] %in% c('rm')) {
        if (dir.exists(argv[2]))
            stop('this is a directory')
        ans <- readline(paste0('Do you really remove ', argv[2], '?',
                               'This cannot be undone. [y/N]'))
        if (ans %in% c('y', 'Y', 'yes', 'Yes'))
            unlink(argv[2])
    } else if (argv[1] %in% c('rmdir')) {
        if (!dir.exists(argv[2]))
            stop('this is not a directory')
        if (length(dir(argv[2])) > 0)
        ans <- readline(paste0(
                'The directory is not empty.',
                'Do you really remove ', argv[2], ' RECURSIVELY?',
                'This cannot be undone. [y/N]'))
        if (ans %in% c('y', 'Y', 'yes', 'Yes'))
            unlink(argv[2], recursive=TRUE)
    } else if (argv[1] %in% c('ls', 'str')) {
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

define_constant_list <- function(){
    list(
        first_wd = getwd(),
        # BUILTIN command Vectors
        builtinv = c('cd', 'dir', 'dir.create', 'echo', 'exit', 'ls',
                     'mkdir', 'print', 'pwd', 'quit', 'rm', 'rmdir', 'setwd'),
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
                       'histogram', 'hline','hex',
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

save_ggplot <- function(exe_statl = list(cmd = 'ggplot(mtcars)+geom_point(aes(cyl,mpg))',
                                         conf= list() ),
                        argv=c('png', 'big')){
    filename <- paste0('tmp.', argv[1])

    # Note: list has builtin partial match.
    # Then size$m calls size$medium.
    size <- list( small = list(w =  480, h =  480),
                 medium = list(w =  960, h =  960),
                    big = list(w = 1960, h = 1440))[[ argv[2] ]]

    if (argv[1] == 'png')
        png(filename, width=size$w, height=size$h)
    else
        pdf(filename)

    dev.off()
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
#' @param dataset A dataframe to attach. Default is NULL.
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
                    copy_to_clipboard(exe_statl$cmd)
                } else if (argv[1] %in% const$savev) {
                    save_ggplot(exe_statl, argv)
                } else { # if 'point' or 'p' is passed
                    exe_statl <- drawgg(dataset, argv)
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
    command <- paste0('ggplot2::geom_', suffix, '()')
    expr <- parse(text = command)
    return(eval(expr)$geom$required_aes)
}

get_possible_aes <- function(suffix='point') {
    command <- paste0('ggplot2::geom_', suffix, '()')
    expr <- parse(text = command)
    geom <- eval(expr)$geom
    possible_aesv <- unique(c(geom$required_aes,
                              geom$non_missing_aes,
                              names(geom$default_aes)))
    return(possible_aesv)
}

find_index <- function(pattern='siz', stringv=c('x', 'y', 'si', 'sh')){
    return(! c(is.na(stringr::str_match(string=stringv, pattern=paste0('^',pattern)))))
}

parse_aes <- function(i, aesv, must_aesv, all_aesv, colnamev){
    # TODO as.factor as.character cut substr
    if (grepl('=', aesv[i])) {
        before_equal <- gsub('=.*', '', aesv[i])
    } else { # no aes specification like geom_point(aes(my_x, my_y))
        before_equal <- must_aesv[i]

        if (i > length(must_aesv))
            stop('too many unspecified aesthetics. ',
                 'Required aesthetics (in order) are: ',
                 paste0(must_aesv, collapse=', '))
    }
    after_equal  <- gsub('.*=',     '', aesv[i])

    if (! before_equal %in% all_aesv)
        before_equal <- all_aesv[find_first(before_equal, all_aesv)] # FIXME refactor

    if (grepl('[0-9]', after_equal))
        after_equal <- colnamev[as.numeric(after_equal)]
    else if (! after_equal %in% colnamev)
        after_equal <- colnamev[find_first(after_equal, colnamev)]
    return(paste0(before_equal, '=', after_equal))
}

#' build a ggplot2 plot and draw it
#'
#' \code{drawgg} interprets given a ggbash character vector
#' and build a complete ggplot2 object.
#'
#' \code{drawgg} is a core function in ggbash library.
#' It performs partial matches against geom name, column names, and aesthetics
#' and construct a complete ggplot2 object as a character.
#' This can be used as a function to write one
#'
#' @param argv A character vector containing ggplot2 geom and aesthetics specifications.
#'             Typically the return value of \code{\link{split_by_space}}.
#' @param dataset A dataframe with attr('ggbash_datasetname').
#' @return A list with the following two fields:
#' \describe{
#'     \item{cmd: }{the \code{eval}uated ggplot2 character.}
#'     \item{conf: }{the parsed aes specifications.}
#' }
#'
#' @examples
#' out <- drawgg(dataset = iris,
#'                    argv = split_by_space("line x=Sepal.W y='Sepal.L"))
#'
#' # copy the built ggplot2 object (Mac OS X)
#' cat(out$cmd, file=(con <- pipe("pbcopy", "w")))
#'
#' @export
drawgg <- function(dataset, argv=c('p','x=2','y=3','colour=4','size=5')){
    if (is.null(dataset))
        stop('dataset is not set')

    const <- define_constant_list()
    # 'p' is resolved into 'point'
    geom_sth <- const$geom_namev[find_first(argv[1], const$geom_namev)]
    message('selected geom: ', geom_sth)

    must_aesv <- get_required_aes(geom_sth)
    all_aesv <- get_possible_aes(geom_sth)
    colnamev <- colnames(dataset)
    message('all_aesv: ', paste0(all_aesv, collapse=' '))

    conf <- list(aes=list())
    aesv <- argv[-1]
    for ( i in seq_along(aesv) ) { # TODO set non-aes elements
        conf$aes[[i]] <- parse_aes(i, aesv, must_aesv, all_aesv, colnamev)
    }
    command <- paste0('ggplot(',attr(dataset, 'ggbash_datasetname'),') ',
                      '+ geom_', geom_sth, '(',
                      'aes(', paste0(conf$aes, collapse = ', '), '))')
    ncmd <- nchar(command) # it's unfair to include labs() characters.
    #command <- paste0(command, ' + labs(subtitle="', command, '")')
    print(eval(parse(text = command)))
    message('executed (', ncmd, ' characters) :\n', command)
    return(list(cmd = command, conf = conf))
}
