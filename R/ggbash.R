show_prompt <- function(dataset_string=' (iris)'){
    username <- Sys.info()['user']
    hostname <- Sys.info()['nodename']
    working_dir <- basename(getwd())
    # if dataset_string is NULL,
    # paste0(dataset_string) is '' (empty string)
    ggbash_prompt <- paste0(username, '@',
                            hostname, ' ',
                            working_dir, dataset_string, ' $')
    return(readline(prompt=ggbash_prompt))
}

split_user_input <- function(input='point 3 4'){
    return(str_split(input, ' ')[[1]])
}

add_input_to_history <- function(input='point 2 3'){
    file1 <- tempfile("Rrawhist")
    savehistory(file1)

    cat(input, '\n', file=file1, append = TRUE)
    loadhistory(file1)
    unlink(file1)
}

execute_builtins <- function(raw_input, argv, const){
    if (argv[1] == 'pwd') {
        message(getwd())
    } else if (argv[1] == 'ls') {
        message( paste(dir(getwd()), collapse='\t') )
        # TODO ls -l
    } else if (argv[1] == 'cd') {
        if (length(argv)<2)
            setwd(const$first_wd)
        else
            setwd(argv[2])
    } else if (argv[1] == 'echo') {
        message(raw_input)
    }
}

load_libraries <- function(){

    librariev <-
        c('stringr', 'dplyr', 'ggplot2')
    lib <- 'stringr' # for test
    for (lib in librariev) {
        if (!suppressWarnings(require(lib, character.only=TRUE))) {
            stop('You need to install library(', lib, ') to execute ggbash.')
        }
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
                       'label', 'map', 'path', 'point', 'polygon',
                       'quantile', 'raster', 'ribbon', 'area', 'rug',
                       'segment', 'curve', 'smooth', 'violin'
        )
        # TODO implement stat like stat_smooth
    )
}

ggbash <- function(){

    load_libraries()

    const <- define_constant_list()

    eval_as_variable <- function(var_string) eval(as.symbol(var_string))
    add_comma <- function(...) paste0(', ', ...)
    dataset <- NULL

    # main loop for command execution
    while (TRUE) {

        raw_input <- 'use iris' # just for test
        raw_input <- show_prompt(dataset$ggbash_datasetname)
        argv <- c('use', 'iris')
        argv <- c('point', '2', '3', 'color=5')
        argv <- split_user_input(raw_input)

        add_input_to_history(raw_input) # add to history even if failed

        if (argv[1] %in% const$builtinv) {

            execute_builtins(raw_input, argv, const)

        } else if (argv[1] %in% c('exit', 'quit')) {

            break

        } else if (argv[1] == 'use') {

            dataset <- tbl_df(eval_as_variable(argv[2]))
            message('attach ', argv[2])
            glimpse(dataset)
            dataset$ggbash_datasetname <- paste0(' (', argv[2], ')')
            # should I store the var name with parentheses?

        } else if (argv[1] == 'show') {

            print(tbl_df(eval_as_variable(argv[2])))

        } else if (argv[1] == 'point') {
            x <- colnames(dataset)[as.numeric(argv[2])]
            y <- colnames(dataset)[as.numeric(argv[3])]
            if (length(argv) > 3) {
                aes_str <- gsub('[0-9]', '', argv[4])
                col_num <- gsub('.*=', '', argv[4])
                color <- add_comma(aes_str, colnames(dataset)[as.numeric(col_num)])
            } else {
                color <- ''
            }
            command <- paste0('ggplot(dataset) + geom_point(aes(',x,', ',y,'',color,'))')
            expr <- parse(text = command)
            print(eval(expr))
            message('executed: ', command)
        }else if (argv[1] %in% geom_namev) {
            x <- colnames(dataset)[as.numeric(argv[2])]
            y <- colnames(dataset)[as.numeric(argv[3])]
            if (length(argv) > 3) {
                aes_str <- gsub('[0-9]', '', argv[4])
                col_num <- gsub('.*=', '', argv[4])
                color <- add_comma(aes_str, colnames(dataset)[as.numeric(col_num)])
            } else {
                color <- ''
            }
            command <- paste0('ggplot(dataset) + geom_point(aes(',x,', ',y,'',color,'))')
            expr <- parse(text = command)
            print(eval(expr))
            message('executed: ', command)
        }
    }
}

