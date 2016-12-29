show_prompt <- function(dataset_string='iris'){
    username <- Sys.info()['user']
    hostname <- Sys.info()['nodename']
    working_dir <- basename(getwd())
    dataset_string <- ifelse(is.null(dataset_string),'', paste0(' (', dataset_string,')'))
    ggbash_prompt <- paste0(username, '@',
                            hostname, ' ',
                            working_dir, dataset_string, ' $')
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

execute_builtins <- function(raw_input, argv, const){
    if (argv[1] == c('pwd', 'getwd')) {
        message(getwd())
    } else if (argv[1] == c('ls', 'dir')) {
        message( paste(dir(getwd()), collapse='\t') )
        # TODO ls -l
    } else if (argv[1] == c('cd', 'setwd')) {
        if (length(argv)<2)
            setwd(const$first_wd)
        else
            setwd(argv[2])
    } else if (argv[1] == c('echo', 'print')) {
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
        geom_namev = c('abline','hline', 'vline', 'bar', 'bin2d',
                       'blank', 'boxplot', 'contour', 'count', 'crossbar',
                       'errorbar', 'linerange', 'pointrange',
                       'density', 'density_2d',
                       'dotplot', 'errorbarh', 'freqpoly', 'hex', 'jitter',
                       'label', 'map', 'path', 'line', 'point', 'polygon',
                       'quantile', 'raster', 'ribbon', 'area', 'rug',
                       'segment', 'curve', 'smooth', 'violin'
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
ggbash <- function(dataset = NULL){

    # initialization
    if (! is.null(dataset))
        attr(dataset, 'ggbash_datasetname') <- deparse(substitute(dataset))
    load_libraries()
    const <- define_constant_list()

    while (TRUE) { tryCatch(
        {   # main loop for command execution

            raw_input <- show_prompt(attr(dataset, 'ggbash_datasetname'))
            argv <- split_user_input(raw_input)

            if (argv[1] %in% const$builtinv) {           execute_builtins(raw_input, argv, const)
            } else if (argv[1] %in% c('exit', 'quit')) { break
            } else if (argv[1] == 'use') {               dataset <- set_dataset(argv)
            } else if (argv[1] == 'show') {              print(tbl_df(eval(as.symbol((argv[2])))))
            } else if (argv[1] %in% const$geom_namev) {  build_ggplot_object(argv, dataset)
            }

        },
        warning = function(wrn) {
                                    message('i got warning')
                                },
          error = function(err) { # stop() goes here
                                    message('ERROR: ', err)
                                },
        finally = {
                    add_input_to_history(raw_input) # add to history even if failed
        }
    ) }
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

    colnamev <- colnames(dataset)

    add_comma <- function(i, ...) ifelse(i==1, paste0(...), paste0(', ', ...))

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
            before_equal <- gsub('[0-9]+', '', aesv[i])
            after_equal  <- gsub('.*=',    '', aesv[i])

            conf$aes[[i]] <- paste0(before_equal, colnamev[as.numeric(after_equal)])
        } else {
        }
    }
    command <- paste0('ggplot(',attr(dataset, 'ggbash_datasetname'),')',
                      ' + geom_', argv[1], '(',
                      'aes(', paste0(conf$aes, collapse = ', '), '))')
    expr <- parse(text = command)
    print(eval(expr))
    message('executed: ', command)

}
