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
    for (i in seq_along(originalv)) {
        short2colname[ out[i] ] <- originalv[i]
    }
    return(short2colname)
}

#' return the first index which contains a given prefix
#'
#' find_first does a prefix partial matching.
#'
#' @param prefix A prefix to be searched
#' @param table A character vector (typically aesthetic name list)
#' @param showWarning Show warning if matched ambiguously. Default is TRUE.
#'
#' @export
find_first <- function(prefix='si',
                       table=c('x', 'y', 'size', 'shape'),
                       showWarning=TRUE){
    indices <- grep(paste0('^', prefix), table)
    if (length(indices)<1 && showWarning) {
        # FIXME refactor (colour and color)
        if (grepl('colo', prefix))
            indices <- grep(paste0('^colour'), table)
        else
            stop('no such prefix')
    }
    if (length(indices)>1 && showWarning)
        message('[ !!! CAUTION !!! ] ambiguous match.',
                ' use "', table[indices][1],
                '" among ', paste0(table[indices], collapse=', '))
    return(indices[1])
}

#' show column index list
#'
#' This function lists all dataset column indices.
#'
#' @param dataset a data frame
#'
#' @seealso \code{partial_unique}, \code{drawgg}
#'
#' @examples
#'
#' # without column indices: explicit
#' drawgg(iris, split_by_space("line x=Sepal.W y=Sepal.L colour=Species"))
#'
#' show_dataset_column_indices(iris)
#'
#' # with column indices: shorter
#' drawgg(iris, split_by_space('l x=2 y=1 c=5'))
#'
#' @export
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

#' show ggbash prompt
#'
#' @param dataset a data frame
#'
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

#' split a given character by a pipe ("|")
#'
#' @param input A character
#'
#' @export
split_by_pipe <- function(input='point x=3 y=4 color=5 | copy'){
    return(strsplit(input, '\\|')[[1]])
}

#' split a given string by spaces
#'
#' @param input A character. Typically one of the elements returned by \code{\link{split_by_pipe}}.
#' @return A character vector
#'
#' @export
split_by_space <- function(input='    point x=3 y=4 color=5 '){
    # remove preceding/trailing spaces
    argv <- strsplit(input, ' ')[[1]]
    return(argv[nchar(argv)>0])
}

#' add ggbash executed commands in R history
#'
#' @param input raw input given to the current ggbash session
#'
#' @importFrom utils savehistory
#' @importFrom utils loadhistory
add_input_to_history <- function(input='point 2 3'){
    history_file <- tempfile("Rhistoryfile")
    savehistory(history_file)

    cat(input, '\n', file=history_file, append = TRUE)
    loadhistory(history_file)
    unlink(history_file)
}

# execute ggbash builtins
execute_ggbash_builtins <- function(raw_input, argv, const, dataset){
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

#' define constant values used in ggbash
#'
#' \code{define_constant_list} has no side effect.
#' It is similar with the 'const' modifier in C or C++.
#'
#' One thing to note is \code{define_constant_list} set implicitly
#' the preference order of geom_name in ggplot2.
#' For example, 'p' ambiguously matches to \code{\link[ggplot2]{geom_point}}
#' and \code{\link[ggplot2]{geom_pointrange}},
#' but ggbash automatically uses \code{\link[ggplot2]{geom_point}}
#' with a warning message about the ambiguity.
#' This is a design choice based on the observation that
#' \code{\link[ggplot2]{geom_point}} is often used
#' more frequently than \code{\link[ggplot2]{geom_pointrange}}.
#' In order to use \code{\link[ggplot2]{geom_pointrange}},
#' at least 6 characters ('pointr') is needed.
#'
#' @seealso The preference order is used
#'          when doing partial match in \code{\link{drawgg}}.
#'
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

#' build a data frame from a data frame name
#'
#' \code{set_ggbash_dataset} receives a character (a data frame name),
#' evaluate it as a symbol, and construct a corresponding tbl_df object.
#' The given character argument is stored in attr('ggbash_datasetname')
#' for future reference in \code{\link{drawgg}}.
#'
#' @param dataset_name a character representing a data frame
#' @param quietly Default is FALSE. Useful for testthat tests
#'
#' @return a tbl_df object with attr('ggbash_datasetname')
#'
#' @seealso \code{\link{drawgg}}
#'
#' @examples
#'
#' newdf <- set_ggbash_dataset('iris')
#' attr(newdf, 'ggbash_datasetname')  # 'iris'
#'
#' @export
set_ggbash_dataset <- function(dataset_name, quietly=FALSE){
    dataset <- dplyr::tbl_df(eval(as.symbol(dataset_name)))
    if (! quietly) {
        message('attach ', dataset_name)
        dplyr::glimpse(dataset)
    }
    attr(dataset, 'ggbash_datasetname') <- dataset_name
    return(dataset)
}

#' copy a given string to clipboard
#'
#' \code{copy_to_clipboard} invokes OS-specific routine to copy a character to clipboard.
#'
#' @param string a character to be copied
#'
#' @return nothing
#'
#' @seealso \code{\link{drawgg}}
#'
#' @export
copy_to_clipboard <- function(
    string='ggplot(mtcars) + geom_point(aes(mpg,cyl))'
){
    os <- Sys.info()['sysname']
    if (os == 'Darwin') {
        cat(string, file=(con <- pipe('pbcopy', 'w')))
        close(con)
    } else if (os == 'Linux') {
        if (! file.exists(Sys.which("xclip")[1]))
            stop("No xclip found")

        cat(string,
            file=(con <- pipe(paste0('xclip -i -selection ', 'clipboard'),
                              "w")))
        close(con)
    } else { # Windows
        cat(string, file='clipboard')
    }
    message('copied to clipboard:\n', string)
}

#' save a ggplot object into a file
#'
#' @param exe_statl A list resulted from \code{\link{drawgg}}
#' @param argv A character vector
#'
#' @importFrom grDevices dev.off
#' @importFrom grDevices png
#' @importFrom grDevices pdf
save_ggplot <- function(exe_statl =
                            list(cmd  = 'ggplot(mtcars) + geom_point(aes(cyl,mpg))',
                                 conf = list('x=cyl', 'y=mpg') ),
                        argv=c('png', 'big')){
    filename <- paste0('tmp.', argv[1])

    # Note: list has builtin partial match for $ (dollar) accessor.
    # size$m calls size$medium.
    size <- list( small = list(w =  480, h =  480),
                 medium = list(w =  960, h =  960),
                    big = list(w = 1960, h = 1440))[[ argv[2] ]]

    if (argv[1] == 'png')
        png(filename, width=size$w, height=size$h)
    else
        pdf(filename)
    #exe_statl$cmd
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
#' @param ambiguous_match A boolean whether to do ambiguous match when a
#'                        ggbash command ambiguously matches several commands.
#'                        Default is TRUE. The matching rules are as follows:
#' @param showWarning Whether to show a warning message
#'                    when ambiguously matched. Default is TRUE.
#' \describe{
#'     \item{Geom name:}{the geom most frequently used (based on my experiences)}
#'     \item{Column name:}{the column with the smallest column index}
#'     \item{Aesthetics:}{required (x, y), non-missing (shape, size), default (alpha, stroke) }
#' }
#' @return nothing
#'
#' @examples
#' \dontrun{ ggbash()
#' ggbash(iris)
#' }
#'
#' @seealso For a oneliner, \code{\link{drawgg}} might be more convenient.
#'
#' @export
ggbash <- function(dataset = NULL, ambiguous_match=TRUE, showWarning=TRUE) {
    # initialization
    if (! is.null(dataset))
        attr(dataset, 'ggbash_datasetname') <- deparse(substitute(dataset))
    const <- define_constant_list()
    exit <- FALSE

    while (TRUE) { tryCatch(
        {   # main loop for command execution

            raw_input <- show_prompt(dataset)
            commandv <- split_by_pipe(raw_input)
            message('commandv: ', paste0(commandv, collapse='-'))
            for (cmd in commandv) {
                argv <- split_by_space(cmd)
                message('argv: ', paste0(argv, collapse='-'))
                if (argv[1] %in% c('exit', 'quit')) {
                    exit <- TRUE
                    break
                } else if (argv[1] == 'use') {
                    dataset <- set_ggbash_dataset(argv[2])
                } else if (argv[1] == 'show') {
                    print(dplyr::tbl_df(eval(as.symbol((argv[2])))))
                } else if (argv[1] %in% const$builtinv) {
                    execute_ggbash_builtins(raw_input, argv, const, dataset)
                } else if (argv[1] %in% c('copy', 'cp')) {
                    copy_to_clipboard(exe_statl$cmd)
                } else if (argv[1] %in% const$savev) {
                    save_ggplot(exe_statl, argv)
                } else { # if 'point' or 'p' is passed
                    exe_statl <- drawgg(dataset, argv, showWarning)
                }

            }
            if (exit)
                break # FIXME an ugly way to avoid returning NULL
        },
        warning = function(wrn) { message('I got warning', wrn) },
          error = function(err) { # stop() comes here
                                  message('ERROR: ', err)
                                },
        finally = { # add to history even if failed for user's next modification
                    add_input_to_history(raw_input)
        }
    ) }
}

#' retrieve required aesthetic names for a given geom
#'
#' @param suffix geom suffix
#'
#' @seealso used in \code{\link{drawgg}}.
#' @export
get_required_aes <- function(suffix='point') {
    command <- paste0('ggplot2::geom_', suffix, '()')
    expr <- parse(text = command)
    return(eval(expr)$geom$required_aes)
}

#' retrieve all aesthetic names for a given geom
#'
#' @param suffix geom suffix
#'
#' @seealso used in \code{\link{drawgg}}.
#' @export
get_possible_aes <- function(suffix='point') {
    command <- paste0('ggplot2::geom_', suffix, '()')
    expr <- parse(text = command)
    geom <- eval(expr)$geom
    possible_aesv <- unique(c(geom$required_aes,
                              geom$non_missing_aes,
                              names(geom$default_aes)))
    return(possible_aesv)
}

#' convert given ggbash strings into ggplot2 aesthetic specifications
#'
#' @param i An integer of index
#' @param aesv A vector of aesthetics
#' @param must_aesv A vector of required aesthetics
#' @param all_aesv A vector of possible aesthetics.
#' @param colnamev A vector of column names of a dataframe.
#' @param showWarning a flag for printing warning when ambiguous match.
#'                    Default is TRUE.
#'
#' @seealso used in \code{\link{drawgg}}.
#' must_aesv and all_aesv are built by
#' \code{\link{get_required_aes}} and
#' \code{\link{get_possible_aes}}, respectively.
#'
parse_ggbash_aes <- function(i, aesv, must_aesv, all_aesv,
                             colnamev, showWarning=TRUE){
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
        before_equal <- all_aesv[find_first(before_equal, all_aesv, showWarning)]

    if (grepl('[0-9]', after_equal))
        after_equal <- colnamev[as.numeric(after_equal)]
    else if (! after_equal %in% colnamev)
        after_equal <- colnamev[find_first(after_equal, colnamev, showWarning)]
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
#' @param showWarning whether to show warning when ambiguously matched. Default is TRUE.
#' @param doEval print the built ggplot object. Default is TRUE. Useful for testthat tests.
#' @return A list with the following two fields:
#' \describe{
#'     \item{cmd: }{the \code{eval}uated ggplot2 character.}
#'     \item{conf: }{the parsed aes specifications.}
#' }
#'
#' @examples
#' out <- drawgg(dataset = iris, argv = split_by_space("line x=Sepal.W y=Sepal.L"))
#'
#' # copy the built ggplot2 object (Mac OS X)
#' copy_to_clipboard(out$cmd)
#'
#' @seealso \code{\link{ggbash}}, \code{\link{copy_to_clipboard}}
#'
#' @export
drawgg <- function(dataset,
                   argv=c('p','x=2','y=3','colour=4','size=5'),
                   showWarning=TRUE,
                   doEval=TRUE){
    if (is.null(dataset))
        stop('dataset is not set')
    if (is.null(attr(dataset, 'ggbash_datasetname'))) # called directly
        dataset <- set_ggbash_dataset(deparse(substitute(dataset)),
                                      quietly=TRUE)

    const <- define_constant_list()
    # 'p' is resolved into 'point'
    geom_sth <- const$geom_namev[find_first(argv[1], const$geom_namev, showWarning)]
    message('selected geom: ', geom_sth)

    must_aesv <- get_required_aes(geom_sth)
    all_aesv <- get_possible_aes(geom_sth)
    colnamev <- colnames(dataset)
    message('all_aesv: ', paste0(all_aesv, collapse=' '))

    conf <- list(aes=list())
    aesv <- argv[-1]
    for ( i in seq_along(aesv) ) { # TODO set non-aes elements
        conf$aes[[i]] <- parse_ggbash_aes(i, aesv, must_aesv,
                                          all_aesv, colnamev, showWarning)
    }
    command <- paste0('ggplot2::ggplot(',attr(dataset, 'ggbash_datasetname'),') ',
                      '+ ggplot2::geom_', geom_sth, '(',
                      'ggplot2::aes(', paste0(conf$aes, collapse = ', '), '))')
    if (doEval)
        print(eval(parse(text = command)))
    command <- gsub('ggplot2::','', command)
    ncmd <- nchar(command) # it's unfair to include labs() characters.
    #command <- paste0(command, ' + labs(subtitle="', command, '")')
    message('executed (', ncmd, ' characters) :\n', command)
    return(list(cmd = command, conf = conf))
}
