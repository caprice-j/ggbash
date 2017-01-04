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
#' @param showWarn Show warning if matched ambiguously. Default is TRUE.
#'
#' @export
find_first <- function(prefix='si',
                       table=c('x', 'y', 'size', 'shape'),
                       showWarn=TRUE){
    indices <- grep(paste0('^', prefix), table)
    if (length(indices)<1 && showWarn) {
        # FIXME refactor (colour and color)
        if (grepl('colo', prefix))
            indices <- grep(paste0('^colour'), table)
        else
            stop('no such prefix: ', prefix)
    }
    if (length(indices)>1 && showWarn &&
        (! prefix %in% c(sapply(1:5, function(i) substr('point',1,i)),
                         sapply(1:4, function(i) substr('line', 1,i))))) {
        warning('Ambiguous match. Use "', table[indices][1],
                '" among ', paste0(table[indices], collapse=', '))
    }
    return(indices[1])
}

#' show column index list
#'
#' This function lists all dataset column indices.
#'
#' @param dataset_str A character representing a data frame
#'
#' @seealso \code{partial_unique}, \code{drawgg}
#'
#' @examples
#'
#' # without column indices: explicit
#' drawgg(iris, list(build_geom(iris, 'point Sepal.Width Sepal.Length')))
#'
#' show_dataset_column_indices('iris')
#'
#' # with column indices: shorter
#' drawgg(iris, list(build_geom(iris, 'point Sepal.Width Sepal.Length')))
#'
#' @export
show_dataset_column_indices <- function(dataset_str=NULL){
    if (is.null(dataset_str))
        return(NULL)
    dataset <- eval(as.symbol((dataset_str)))

    pad <- function(i, width=4, side='') {
        gsub('#',' ', sprintf(paste0('%',side,'#',width,'s'), i))
    }

    nchar_longest <- max(sapply(colnames(dataset), nchar))
    short_colnamel <- partial_unique(colnames(dataset), i=4)
    # i = 4 because too short colnames are hard to read
    mod <- ifelse(ncol(dataset)>50, 15, 5)
    linev <- rep('', mod)
    for ( i in seq_along(short_colnamel) ) {
        this <- names(short_colnamel)[i]
        index <- ((i-1) %% mod) + 1
        linev[index] <- paste0(linev[index],
                               pad(i, width=nchar(ncol(dataset))), ': ',
                               pad(this, width=nchar_longest, side='-'), '\t')
    }

    for (i in 1:mod ){
        if (linev[i]!='')
            message(linev[i])
    }
}

#' build a ggbash prompt string
#'
build_prompt <- function() {
    username <- Sys.info()['user']
    hostname <- Sys.info()['nodename']
    working_dir <- basename(getwd())
    ggbash_prompt <- paste0(username, '@',
                            hostname, ' ',
                            working_dir, ' $ ')
    return(ggbash_prompt)
}

#' show ggbash prompt
#'
show_prompt <- function() {
    # MAYBE-LATER how can I test functions having readline()?
    return(readline(prompt=build_prompt()))
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
    return(argv[nchar(argv) > 0])
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

#' execute ggbash builtins
#'
#' @param raw_input A character of ggbash command chain (might contain pipes)
#' @param argv A character vector
#' @param const A list of ggbash constants
#'              returned by \{code{define_constant_list}.
#'
execute_ggbash_builtins <- function(raw_input, argv, const){
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
    } else if (argv[1] %in% c('list', 'str')) {
        show_dataset_column_indices(argv[2])
    } else if (argv[1] %in% c('ls', 'dir')) {
        message( paste(dir(getwd()), collapse='\t') )
        # TODO ls -l
    } else if (argv[1] %in% c('cd', 'setwd')) {
        if (length(argv)<2)
            setwd(const$first_wd)
        else
            setwd(argv[2])
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
        # Note: the following commands are not included -- see exec_ggbash
        #       echo print quit exit
        builtinv = c('cd', 'dir', 'dir.create', 'ls', 'list',
                     'mkdir', 'pwd', 'rm', 'rmdir', 'setwd'),
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
                       'rect', 'rug', 'raster', 'ribbon',
                       'segment', 'smooth', 'step',
                       'text', 'tile',
                       'vline', 'violin'
        ),
        savev = c('png', 'pdf')
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
#' @param dataset_name a character representing a data frame.
#'                     If a matrix is given, it's transformed into a data frame.
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
set_ggbash_dataset <- function(dataset_name='iris'){
    rect_data <- eval(as.symbol(dataset_name), envir = .GlobalEnv)
    if (class(rect_data) == 'matrix')
        rect_data <- as.data.frame(rect_data)
    dataset <- dplyr::tbl_df(rect_data)
    attr(dataset, 'ggbash_datasetname') <- dataset_name
    return(dataset)
}

#' set ggplot() aesthetics arguments
#'
#' @param dataset A data frame
#' @param aesv A vector of aesthetics
#'
set_ggplot_default_aes <- function(dataset, aesv = c('x=mpg', 'y=cyl', 'c=am')) {

    # ggplot2::ggplot() seems only interptets aes (but no non-aes)
    if (length(aesv) == 0) {
        attr(dataset, 'ggbash_ggplot2') <- ''
        return(dataset)
    }

    aes_str <- ''
    all_aesv <- get_possible_aes('point') # FIXME all geoms

    for(i in seq_along(aesv)) {
        aes_str <- paste0(aes_str, ifelse(i>1,', ', ''),
                          parse_ggbash_aes(i, aesv,
                                           all_aesv,
                                           must_aesv = c(),
                                           colnamev=colnames(dataset)))
    }

    attr(dataset, 'ggbash_ggplot2') <- paste0('aes(', aes_str, ')')

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
    message('copied to clipboard:\n  ', string)
}

build_ggbash_filename <- function(
    conf = list(aes = list('x=cyl', 'y=mpg'),
                non_aes = list('color="blue"', 'shape="18"') ),
    out = list(filename = NA, w = 960/72, h = 960/72, dpi=72, dir='./'),
    extension='png'
){

    if (length(conf$non_aes) > 0) {
        tmp <- gsub(paste0('"', '|', "'"), '', conf$non_aes)
        quote_stripped <- paste0('_', gsub('=', '-', tmp), collapse='_')
    } else {
        quote_stripped <- ''
    }

    geom_string <- paste0(sort(conf$geom), collapse='-')

    aes_string <- paste0(sort(gsub('=', '-', conf$aes)), collapse='_')

    return(
        paste0(geom_string, '_', aes_string, quote_stripped,
               '.', out$w*out$dpi, 'x', out$h*out$dpi, '.', extension)
    )
}

#' parse given plot settings
#'
#' @param argv A character vector
#' @param conf A list of aesthetic and non-aesthetic assignments
#' @param dataset_string A character representing a dataset directory
parse_plot_attributes <- function(
    argv = c('png', '"myname"', '900x640', 'my_plot_dir/'),
    conf = list(aes = list('x=cyl', 'y=mpg'),
            non_aes = list('color="blue"', 'shape="18"')),
    dataset_string = 'mtcars-32'
){
    dpi <- 72
    out <- list(filename = NA, filepath = NA,
                w = 960/dpi, h = 960/dpi, dpi=dpi, dir='./')
    # 72 pixels per inch is R's default
    single_quote <- "'"
    double_quote <- '"'
    for (a in argv[-1]) {
        if (grepl(single_quote, a) ||
            grepl(double_quote, a)) { # filename
            out$filename <-
                paste0(gsub(paste0(single_quote, '|', double_quote), '', a),
                       '.', argv[1])
        } else if (grepl('/', a)) {
            out$dir <- paste0(out$dir, a)
        } else if (grepl('[0-9]', a) && grepl('x', a)) { # size (numeric)
            size <- as.numeric(strsplit(a, 'x')[[1]])
            out$w <- ifelse(size[1] > 50, size[1]/dpi, size[1])
            out$h <- ifelse(size[2] > 50, size[2]/dpi, size[2])
        } else {
            index <- find_first(a, c('small', 'big'))
            selected <-
                list(small   = list(w =  480, h =  480),
                     big     = list(w = 1960, h = 1440))[[ index ]]
            out$w <- selected$w / dpi
            out$h <- selected$h / dpi
        }
    }

    if (is.na(out$filename)) # auto-assign
        out$filename <- build_ggbash_filename(conf, out, argv[1])
    # FIXME multiple same aes (i.e. point x=Pt | smooth x=Age )

    out$filepath <- paste0(out$dir, dataset_string, '/', out$filename)

    return(out)
}

#' save a ggplot object into a file
#'
#' @param dataset_string A character. Used as a directory.
#' @param ggstr A list resulted from \code{\link{drawgg}}
#' @param argv A character vector
#'
#' @importFrom grDevices dev.off
#' @importFrom grDevices png
#' @importFrom grDevices pdf
save_ggplot <- function(
    dataset_string = 'mtcars-32',
    ggstr =
        list(cmd  = 'ggplot2::ggplot(mtcars) + ggplot2::geom_point(ggplot2::aes(cyl,mpg))',
             conf = list('x=cyl', 'y=mpg') ),
    argv=c('png', '200x500', '"my-file-name"', 'my_plot_dir/')
){
    attrl <- parse_plot_attributes(argv, ggstr$conf, dataset_string)
    dir.create(attrl$dir, showWarnings=FALSE)
    oldwd <- setwd(attrl$dir)
    on.exit()
    dir.create(dataset_string, showWarnings=FALSE)
    setwd(dataset_string)
    setwd(oldwd)

    ggplot2::ggsave(attrl$filepath, plot=eval(parse(text=ggstr$cmd)),
                    width=attrl$w, height=attrl$h, units='in', dpi=attrl$dpi)
    message('saved: ', attrl$filepath)
}

#' execute raw ggbash commands
#'
#' @param raw_input A ggbash command chain (might contain pipes)
#' @param showWarn Whether to show a warning message
#'                    when ambiguously matched. Default is TRUE.
#' @param batchMode Default is FALSE.
#'                  If TRUE, the resulted ggplot object is returned.
#'
#' @export
exec_ggbash <- function(raw_input='gg iris | point 1 2 | copy',
                        showWarn=TRUE, batchMode=FALSE){
    const <- define_constant_list()
    commandv <- split_by_pipe(raw_input)
    geom_list <- list()
    i <- 1
    for (cmd in commandv) {
        argv <- split_by_space(cmd)
        if (grepl(paste0('^', argv[1]), 'ggplot2')) {
            # partial prefix match to 'ggplot2'
            dataset <- set_ggbash_dataset(argv[2])
            dataset <- set_ggplot_default_aes(dataset,
                                              argv[c(-1,-2)])
        } else if (argv[1] == 'show') {
            print(dplyr::tbl_df(eval(as.symbol((argv[2])))))
            return(FALSE)
        } else if (argv[1] %in% c('echo', 'print')) {
            if (length(geom_list) > 0)
                ggstr <- drawgg(dataset, geom_list, doEval=FALSE)
            message(ifelse(exists('ggstr'), ggstr$cmd, argv[2]))
            return(FALSE)
        } else if (argv[1] %in% const$builtinv) {
            execute_ggbash_builtins(raw_input, argv, const)
        } else if (argv[1] %in% c('copy', 'cp')) {
            ggstr <- drawgg(dataset, geom_list, doEval=FALSE)
            copy_to_clipboard(ggstr$cmd)
        } else if (argv[1] %in% const$savev) {
            dataset_str <- paste0(attr(dataset, 'ggbash_datasetname'),
                                  '-', nrow(dataset))
            ggstr <- drawgg(dataset, geom_list, doEval=FALSE)
            save_ggplot(dataset_str, ggstr, argv)
        } else if (argv[1] %in% c('exit', 'quit', 'q')) {
                return(TRUE)
        } else { # if 'point' or 'p' is passed
            geom_list[[i]] <- build_geom(dataset, cmd, showWarn)
            i <- i + 1
        }
    }
    if (batchMode)
        return(drawgg(dataset, geom_list))
    drawgg(dataset, geom_list)
    return(FALSE)
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
#' If you give a string as a first argument of `ggbash`,
#' ggbash will exit just after executing the command. Useful for a one-liner.
#'
#' @param batch A character. If given, \code{ggbash()} will exit
#'              just after executing the given command.
#' @param clipboard Default is NULL
#'                  If batch is non-empty and clipboard is non-NULL,
#'                  ggbash copies a resulted ggplot2 object to clipboard.
#'              just after executing the given command.
#' @param showWarn Whether to show a warning message
#'                    when ambiguously matched. Default is TRUE.
#' \describe{
#'     \item{Geom name:}{the geom most frequently used (based on my experiences)}
#'     \item{Column name:}{the column with the smallest column index}
#'     \item{Aesthetics:}{required (x, y), non-missing (shape, size), default (alpha, stroke) }
#' }
#' @return nothing
#'
#' @examples
#' \dontrun{ ggbash() # enter into an interactive ggbash session
#' }
#' # plot a ggplot2 figure
#' ggbash('gg iris | point Petal.Width Petal.Length')
#'
#' #' # plot a ggplot2 figure and copy the result
#' ggbash('gg iris | point Petal.Width Petal.Length', 1)
#'
#' @export
ggbash <- function(batch='', clipboard=NULL, showWarn=TRUE) {
    if (batch != '') {
        if (! is.null(clipboard))
            batch <- ifelse(grepl(batch, '|\\s*copy'),
                            batch, paste0(batch,' | copy'))
        return(exec_ggbash(fstrings::fstring(batch), showWarn, batchMode=TRUE))
    }
    while (TRUE) { tryCatch(
        {   raw_input <- show_prompt()
            if (exec_ggbash(fstrings::fstring(raw_input), showWarn))
                break
        },
        warning = function(wrn) { message('I got warning', wrn) },
          error = function(err) { message('ERROR: ', err) }, # stop() to here
        finally = { add_input_to_history(raw_input) } # add even if failed
    )}
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
#' @param showWarn a flag for printing warning when ambiguous match.
#'                    Default is TRUE.
#'
#' @seealso used in \code{\link{drawgg}}.
#' must_aesv and all_aesv are built by
#' \code{\link{get_required_aes}} and
#' \code{\link{get_possible_aes}}, respectively.
#'
parse_ggbash_aes <- function(i, aesv, must_aesv, all_aesv,
                             colnamev, showWarn=TRUE){
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
        before_equal <- all_aesv[find_first(before_equal, all_aesv, showWarn)]

    if (! after_equal %in% colnamev)
        after_equal <- colnamev[find_first(after_equal, colnamev, showWarn)]
    return(paste0(before_equal, '=', after_equal))
}

#'  convert given ggbash strings into ggplot2 non-aesthetic (constant) specifications
#'
#' @param non_aes A character of a non-aesthetic key and value pair
#' @param all_aesv A vector of possible aesthetics.
#' @param showWarn a flag for printing warning when ambiguous match.
#'                    Default is TRUE.
#'
#' @seealso used in \code{\link{drawgg}}.
#' all_aesv are built by \code{\link{get_possible_aes}}.
#' \code{\link{parse_ggbash_aes}}
#'
parse_ggbash_non_aes <- function(non_aes='shape=1', all_aesv,
                                 showWarn=TRUE){
    before_equal <- gsub('=.*', '', non_aes)
    after_equal  <- gsub('.*=',     '', non_aes)

    if (! before_equal %in% all_aesv) # partial match
        before_equal <- all_aesv[find_first(before_equal, all_aesv, showWarn)]

    return(paste0(before_equal, '=', after_equal))
}

#' build a geom (a Layer object) from a ggbash character
#'
#' @param dataset A dataframe with attr('ggbash_datasetname').
#' @param ggstr A ggbash string of geom name, aesthetics, and non-aesthetics
#' @param showWarn Show \code{warning} message when ambiguous partial match.
#'                 Default is TRUE.
#'
#' \code{build_geom} interprets given a ggbash character (vector)
#' and build a geom string.
#'
#' \code{build_geom} is a core function in ggbash library.
#' It performs partial matches against geom name, column names, and aesthetics
#' and construct a complete geom as a character.
#'
#' @return A list with the following two fields:
#' \describe{
#'     \item{geomstr: }{A string representing a geom.}
#'     \item{geomstr_verbose: }{geomstr with ggplot2:: modifiers.}
#'     \item{conf: }{the parsed geom/aes/non-aes specifications.}
#' }
#'
#' @examples
#' build_geom(iris, 'p Sepal.Width Sepal.Length color=Species shape=13')
#'
#' @seealso \code{\link{ggbash}}, \code{\link{copy_to_clipboard}}
#'
#' @export
build_geom <- function(
    dataset, ggstr='p x=2 y=3 colour=4 shape="18"', showWarn=TRUE
){
    if (is.null(attr(dataset, 'ggbash_datasetname'))) # called directly
        dataset <- set_ggbash_dataset(deparse(substitute(dataset)))
    argv <- split_by_space(ggstr)

    const <- define_constant_list()
    single_quote <- "'"
    double_quote <- '"'
    # 'p' is resolved into 'point'
    geom_sth <- const$geom_namev[find_first(argv[1], const$geom_namev, showWarn)]

    must_aesv <- get_required_aes(geom_sth)
    all_aesv <- get_possible_aes(geom_sth)
    colnamev <- colnames(dataset)

    is_aes <- ! grepl(paste0(single_quote, '|', double_quote, '|[0-9]'), argv)
    aesv <- argv[is_aes][-1]
    non_aesv <- argv[!is_aes]
    conf <- list(geom=geom_sth, aes=rep(NA, length(aesv)),
                 non_aes=rep(NA, length(non_aesv)))
    for ( i in seq_along(aesv) ) { # TODO set non-aes elements
        conf$aes[i] <- parse_ggbash_aes(i, aesv, must_aesv,
                                          all_aesv, colnamev, showWarn)
    }
    for ( i in seq_along(non_aesv) ) { # TODO set non-aes elements
        conf$non_aes[i] <- parse_ggbash_non_aes(non_aesv[i], all_aesv, showWarn)
    }

    aes_str <- paste0('ggplot2::aes(', paste0(conf$aes, collapse = ', '), ')')
    non_aes_str <- paste0(ifelse(length(conf$non_aes),', ',''),
                          paste0(conf$non_aes, collapse = ', '))

    command <- paste0('ggplot2::geom_', geom_sth, '(', aes_str, non_aes_str, ')')
    return(list(geomstr = gsub('ggplot2::','', command),
                geomstr_verbose = command, conf = conf))
}

#' build a complete ggplot2 object and print it
#'
#' \code{drawgg} interprets given a ggbash character vector
#' and build a complete ggplot2 object.
#'
#' \code{drawgg} is a core function in ggbash library.
#' It performs partial matches against geom name, column names, and aesthetics
#' and construct a complete ggplot2 object as a character.
#' This can be used as a function to write one
#'
#' @param dataset A dataframe with attr('ggbash_datasetname').
#' @param geom_list a list of \code{\link{build_geom}} results.
#' @param doEval print the built ggplot object. Default is TRUE. Useful for testthat tests.
#' @return A list with the following two fields:
#' \describe{
#'     \item{cmd: }{the \code{eval}uated ggplot2 character.}
#'     \item{cmd_verbose: }{cmd with ggplot2:: modifiers and \code{labs}.}
#'     \item{conf: }{the parsed geom/aes/non-aes specifications.}
#' }
#'
#' @examples
#' geom_list <- list(build_geom(iris, 'p Sepal.W Sepal.L'), build_geom(iris, 'l Sepal.W Sepal.L'))
#' out <- drawgg(dataset = iris, geom_list = geom_list)
#'
#' copy_to_clipboard(out$cmd)
#'
#' @seealso \code{\link{ggbash}}, \code{\link{copy_to_clipboard}}, \code{\link{build_geom}}
#'
#' @export
drawgg <- function(
    dataset=NULL,
    geom_list = list(
        list(geomstr = 'geom_point(aes(x=mpg,y=cyl))',
             geomstr_verbose =
                 'ggplot2::geom_point(ggplot2::aes(x=mpg,y=cyl), colour="blue", size=6)',
             conf = list(aes=c('x=mpg', 'y=cyl'),
                         non_aes=c('colour="blue"', 'size=6'))),
        list(geomstr = 'geom_line(aes(x=mpg,y=cyl))',
             geomstr_verbose =
                 'ggplot2::geom_line(ggplot2::aes(x=mpg,y=cyl), colour="blue", size=6)',
             conf = list(aes=c('x=mpg', 'y=cyl'),
                         non_aes=c('colour="blue"', 'size=6')))),
    doEval=TRUE
){
    if (is.null(dataset))
        return()
    if (is.null(attr(dataset, 'ggbash_datasetname'))) # called directly
        dataset <- set_ggbash_dataset(deparse(substitute(dataset)))

    aes_str <- ifelse(attr(dataset, 'ggbash_ggplot2') == '',
                      '',
                      paste0(', ', attr(dataset, 'ggbash_ggplot2')))

    gg <- paste0('ggplot2::ggplot(',
                 attr(dataset, 'ggbash_datasetname'), aes_str, ')')
    conf <- list(geom=c(), aes=c(), non_aes=c())

    for (geom in geom_list) {
        gg <- paste0(gg, ' + ', geom$geomstr_verbose)
        conf$aes <- c(conf$aes, geom$conf$aes)
        conf$non_aes <- c(conf$non_aes, geom$conf$non_aes)
        conf$geom <- c(conf$geom, geom$conf$geom)
    }
    if (doEval)
        print(eval(parse(text = gg)))
    short_cmd <- gsub('ggplot2::','', gg)
    return(list(cmd = short_cmd, cmd_verbose = gg, conf = conf))
}
