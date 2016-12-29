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
                               str_pad(i,
                                       width=nchar(ncol(dataset))), ': ',
                               str_pad(this,
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

split_user_input <- function(input='point x=3 y=4 color=5'){
    return(str_split(input, ' ')[[1]])
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

load_libraries <- function(){

    lib <- 'stringr' # for test
    for (lib in c('stringr', 'dplyr', 'ggplot2')) {
        if (!suppressWarnings(require(lib, character.only=TRUE)))
            stop('You need to install library(', lib, ') to execute ggbash.')
    }
}

define_constant_list <- function(){
    list(
        first_wd = getwd(),
        # BUILTIN command Vectors
        builtinv = c('cd', 'echo', 'exit', 'ls', 'pwd', 'quit'),
        # all geom in ggplot2 documents
        geom_namev = c('abline', 'area',
                       'bar', 'bin2d', 'blank', 'boxplot',
                       'curve', 'contour', 'count', 'crossbar',
                       'density', 'density_2d', 'dotplot',
                       'errorbar', 'errorbarh',
                       'freqpoly',
                       'hex', 'hline',
                       'jitter',
                       'label', 'line', 'linerange',
                       'map',
                       'path', 'point', 'polygon', # 'pointrange', # I believe no one use pointrange
                       'quantile',
                       'raster', 'ribbon', 'rug',
                       'segment', 'smooth',
                       'violin', 'vline'
        )
        # TODO implement stat like stat_smooth
    )
}

set_dataset <- function(argv){
    dataset <- tbl_df(eval(as.symbol(((argv[2])))))
    message('attach ', argv[2])
    glimpse(dataset)
    attr(dataset, 'ggbash_datasetname') <- argv[2]
    # should I store the var name with parentheses?
    return(dataset)
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
#' @param dataset a dataframe to attach (default is NULL).
#'                You can define the dataframe later
#'                by 'use your_dataset' command in the ggbash session.
#'                If a matrix object is given, It's automatically
#'                converted into a tbl_df object.
#' @return nothing
#' @examples
#' ggbash()
#' ggbash(iris)
ggbash <- function(dataset = NULL, partial_match=TRUE) {

    # initialization
    if (! is.null(dataset))
        attr(dataset, 'ggbash_datasetname') <- deparse(substitute(dataset))
    load_libraries()
    const <- define_constant_list()

    while (TRUE) { tryCatch(
        {   # main loop for command execution

            raw_input <- show_prompt(dataset)
            argv <- split_user_input(raw_input)

            if (argv[1] %in% c('exit', 'quit')) {
                break
            } else if (argv[1] == 'use') {
                dataset <- set_dataset(argv)
            } else if (argv[1] == 'show') {
                print(tbl_df(eval(as.symbol((argv[2])))))
            } else if (argv[1] %in% const$geom_namev) {
                build_ggplot_object(argv, dataset)
            } else if (argv[1] %in% const$builtinv) {
                execute_builtins(raw_input, argv, const, dataset)
            }

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

build_ggplot_object <- function(argv=c('point','x=2','y=3','color=4','size=5'), dataset){

    if (is.null(dataset))
        stop('dataset is not set')

    # three cases:
    # 1. no name just 1 2 3
    # 2. color=3 size=4
    # 3. c=4      (partial match)

    # x
    # y
    # alpha
    # colour
    # fill
    # shape
    # size
    # stroke

    required_aesv <- get_required_aes(argv[1])

    colnamev <- colnames(dataset)

    add_comma <- function(i, ...) ifelse(i==1, paste0(...), paste0(', ', ...))

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
        if (grepl('[0-9]', after_equal))
            after_equal <- colnamev[as.numeric(after_equal)]
        else if (! after_equal %in% colnamev)
            after_equal <- short2colname[[after_equal]]

        conf$aes[[i]] <- paste0(before_equal, '=', after_equal)
    }
    print(conf$aes)
    command <- paste0('ggplot(',attr(dataset, 'ggbash_datasetname'),')',
                      ' + geom_', argv[1], '(',
                      'aes(', paste0(conf$aes, collapse = ', '), '))')
    expr <- parse(text = command)
    print(eval(expr))
    message('executed: ', command)

}
