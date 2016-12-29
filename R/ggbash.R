show_prompt <- function(dataset_string=' (iris)'){
    username <- Sys.info()['user']
    hostname <- Sys.info()['nodename']
    working_dir <- basename(getwd())
    # if dataset_string is NULL, paste0(dataset_string) is '' (empty string)
    ggbash_prompt <- paste0(username, '@', hostname, ' ', working_dir, dataset_string, ' $')
    return(readline(prompt=ggbash_prompt))
}

parse_user_input <- function(input = 'point 3 4'){
    return(str_split(input, ' ')[[1]])
}

add_input_to_history <- function(input = 'point 2 3'){
    file1 <- tempfile("Rrawhist")
    savehistory(file1)

    cat(input, '\n', file=file1, append = TRUE)
    loadhistory(file1)
    unlink(file1)
}

ggbash <- function(){
    message('ggbash version 0.1')
    library(stringr)
    library(dplyr)
    library(ggplot2)

    eval_as_variable <- function(varname_string) eval(as.symbol(varname_string))
    dataset <- NULL
    dataset_string <- NULL
    while (TRUE) {
        raw_input <- show_prompt(dataset_string)
        # just for test
            argv <- c('use', 'iris')
            argv <- c('point', '2', '3')
        argv <- parse_user_input(raw_input)

        add_input_to_history(raw_input)

        if (argv[1] == 'use') {
            dataset <- tbl_df(eval_as_variable(argv[2]))
            message('use ', argv[2])
            glimpse(dataset)
            dataset_string <- paste0(' (', argv[2], ')')
        } else if (argv[1] == 'point') {
            x <- colnames(dataset)[as.numeric(argv[2])]
            y <- colnames(dataset)[as.numeric(argv[3])]
            if (length(argv) > 3)
                color <- colnames(dataset)[as.numeric(argv[4])]
            command <- paste0('ggplot(dataset) + geom_point(aes(',x,',',y,'))')
            expr <- parse(text = command)
            print(eval(expr))
            message('executed: ', command)
        }else if (argv[1] %in% c('exit', 'quit')) {
            break
        }
    }
}

