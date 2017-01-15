#' @import ggplot2
NULL

#' show column index list
#'
#' This function lists all dataset column indices.
#'
#' @param dataset_str A character representing a data frame
#'
#' @seealso \code{partial_unique}
#'
#'
#' @export
show_dataset_column_indices <- function(dataset_str=NULL){
    if (is.null(dataset_str))
        return(NULL)
    dataset <- eval(as.symbol(dataset_str))

    pad <- function(i, width=4, side="") {
        gsub("#", " ", sprintf(paste0("%", side, "#", width, "s"), i))
    }

    nchar_longest <- max(sapply(colnames(dataset), nchar))
    short_colnamel <- partial_unique(colnames(dataset), i = 4)
    # i = 4 because too short colnames are hard to read
    mod <- ifelse(ncol(dataset) > 50, 15, 5)
    linev <- rep("", mod)
    for ( i in seq_along(short_colnamel) ) {
        this <- names(short_colnamel)[i]
        index <- ( (i - 1) %% mod ) + 1
        linev[index] <-
            paste0(linev[index],
                   pad(i, width = nchar(ncol(dataset))), ": ",
                   pad(this, width = nchar_longest, side = "-"), "\t")
    }

    for (i in 1:mod ){
        if (linev[i] != "")
            message(linev[i])
    }
}

#' build a ggbash prompt string
#'
build_prompt <- function() {
    username <- Sys.info()["user"]
    hostname <- Sys.info()["nodename"]
    working_dir <- basename(getwd())
    ggbash_prompt <- paste0(username, "@",
                            hostname, " ",
                            working_dir, " $ ")
    return(ggbash_prompt)
}

show_prev_colnames <- function() {
    pre2full <- partial_unique(ggbashenv$colname)
    prefix <- names(pre2full)
    suffix <- c()
    for (i in seq_along(prefix))
        suffix <- c(suffix, gsub(paste0("^", prefix[i]),'', pre2full[[i]]))
    colnamev <- paste0(prefix, "(", suffix, ")")
    message("cols: ", paste0(colnamev, collapse="\t"))
}

#' show ggbash prompt
#'
show_prompt <- function() {
    if (! is.null(ggbashenv$colname))
        show_prev_colnames()
    # MAYBE-LATER how can I test functions having readline()?
    return(readline(prompt = build_prompt()))
}

#' split a given character by a pipe ("|")
#'
#' @param input A character
#'
#' @export
split_by_pipe <- function(input="point x=3 y=4 color=5 | copy"){
    return(strsplit(input, "\\|")[[1]])
}

#' split a given string by spaces
#'
#' @param input A character. Typically one of the elements returned by \code{\link{split_by_pipe}}.
#' @return A character vector
#'
#' @export
split_by_space <- function(input="    point x=3 y=4 color=5 "){
    # remove preceding/trailing spaces
    noparen <- gsub("\\(|\\)", " ", input)
    argv <- strsplit(noparen, " ")[[1]]
    return(argv[nchar(argv) > 0])
}

#' add ggbash executed commands in R history
#'
#' @param input raw input given to the current ggbash session
#'
#' @importFrom utils savehistory
#' @importFrom utils loadhistory
add_input_to_history <- function(input="point 2 3"){
    history_file <- tempfile("Rhistoryfile")
    savehistory(history_file)

    cat(input, "\n", file = history_file, append = TRUE)
    loadhistory(history_file)
    unlink(history_file)
}

#' execute ggbash builtins
#'
#' @param raw_input A character of ggbash command chain (might contain pipes)
#' @param argv A character vector
#' @param const A list of ggbash constants
#'              returned by \{code{define_ggbash_constants}.
#'
execute_ggbash_builtins <- function(raw_input, argv, const){
    if (argv[1] %in% c("pwd", "getwd")) {
        message(getwd())
    } else if (argv[1] %in% c("mkdir", "dir.create")) {
        dir.create(argv[2], recursive = TRUE)
    } else if (argv[1] %in% c("rm")) {
        if (dir.exists(argv[2]))
            stop("this is a directory")
        ans <- readline(paste0("Do you really remove ", argv[2], "?",
                               "This cannot be undone. [y/N]"))
        if (ans %in% c("y", "Y", "yes", "Yes"))
            unlink(argv[2])
    } else if (argv[1] %in% c("rmdir")) {
        if (!dir.exists(argv[2]))
            stop("this is not a directory")
        if (length(dir(argv[2])) > 0)
        ans <- readline(paste0(
                "The directory is not empty.",
                "Do you really remove ", argv[2], " RECURSIVELY?",
                "This cannot be undone. [y/N]"))
        if (ans %in% c("y", "Y", "yes", "Yes"))
            unlink(argv[2], recursive = TRUE)
    } else if (argv[1] %in% c("list", "str")) {
        show_dataset_column_indices(argv[2])
    } else if (argv[1] %in% c("ls", "dir")) {
        if (length(argv) > 1 && argv[2] == "-l")
            message( paste(dir(getwd()), collapse = "\n") )
        else
            message( paste(dir(getwd()), collapse = "\t") )
    } else if (argv[1] %in% c("cd", "setwd")) {
        if (length(argv) < 2)
            setwd(const$first_wd)
        else
            setwd(argv[2])
    }
}

#' build a data frame from a data frame name
#'
#' \code{set_ggbash_dataset} receives a character (a data frame name),
#' evaluate it as a symbol, and construct a corresponding tbl_df object.
#'
#' @param dataset_name a character representing a data frame.
#'                     If a matrix is given, it's transformed into a data frame.
#'
#' @return a tbl_df object with attr('ggbash_datasetname')
#'
#' @examples
#'
#' newdf <- set_ggbash_dataset('iris')
#' attr(newdf, 'ggbash_datasetname')  # 'iris'
#'
#' @export
set_ggbash_dataset <- function(dataset_name="iris+point"){

    dataset_name <- gsub("\\+.*", "", dataset_name)
    dataset_name <- gsub(",", "", dataset_name)

    if (! exists(dataset_name))
        stop("[E001] No such dataset: ", dataset_name)
    rect_data <- eval(as.symbol(dataset_name), envir = .GlobalEnv)
    if (class(rect_data)[1] == "matrix")
        rect_data <- as.data.frame(rect_data)
    dataset <- tibble::as_data_frame(rect_data)
    attr(dataset, "ggbash_datasetname") <- dataset_name
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
#' @export
copy_to_clipboard <- function(
    string="ggplot(mtcars) + geom_point(aes(mpg,cyl))"
){
    os <- Sys.info()["sysname"]
    if (os == "Darwin") {
        cat(string, file = (con <- pipe("pbcopy", "w")))
        close(con)
    } else if (os == "Linux") {
        if (! file.exists(Sys.which("xclip")[1]))
            stop("No xclip found")

        cat(string,
            file = (con <- pipe(paste0("xclip -i -selection ", "clipboard"),
                              "w")))
        close(con)
    } else {
        cat(string, file = "clipboard")  # Windows
    }
    message("copied to clipboard:\n  ", string)
}

build_ggbash_filename <- function(
    conf = list(aes = c("x=cyl", "y=mpg"),
                non_aes = c("color='blue'", "shape='18'"),
                geom_list = c("point")),
    out = list(filename = "", dir = "./",
               w = 960 / 72, h = 960 / 72, dpi = 72),
    extension="png"
){

    if (length(conf$non_aes) > 0) {
        tmp <- gsub(paste0("\\\"|'"), "", conf$non_aes)
        quote_stripped <- paste0("_", gsub("=", "-", tmp), collapse = "_")
    } else {
        quote_stripped <- ""
    }

    non_null <- ! is.null(conf$geom_list)
    if (non_null && conf$geom_list != "") {
        geom_string <- paste0(sort(conf$geom_list), collapse = "-")
    } else {
        geom_string <- "no_geom"
    }

    aes_string <- paste0(sort(gsub("=", "-", conf$aes)), collapse = "_")

    return(
        paste0(geom_string, "_", aes_string, quote_stripped,
               ".", out$w * out$dpi, "x", out$h * out$dpi, ".", extension)
    )
}

#' parse given plot settings
#'
#' @param argv A character vector
#' @param conf A list of aesthetic and non-aesthetic assignments
#' @param dataset_string A character representing a dataset directory
parse_plot_attributes <- function(
    argv = c("png", "'myname'", "900*640", "my_plot_dir"),
    conf = list(aes = list("x=cyl", "y=mpg"),
            non_aes = list("color='blue'", "shape=18"),
            geom_list = c("point", "smooth")),
    dataset_string = "mtcars-32"
){
    dpi <- 72
    out <- list(filename = "", filepath = "",
                w = 960 / dpi, h = 960 / dpi, dpi = dpi, dir = "./")
    # 72 pixels per inch is R"s default
    single_quote <- "'"
    double_quote <- "\\\""
    for (a in argv[-1]) {
        if (grepl(single_quote, a) ||
            grepl(double_quote, a)) {
            out$filename <-
                paste0(gsub(paste0(single_quote, "|", double_quote), "", a),
                       ".", argv[1])
        } else if (grepl("\\*", a)) {
            size <- as.numeric(strsplit(a, "\\*")[[1]])
            out$w <- ifelse(size[1] > 50, size[1] / dpi, size[1])
            out$h <- ifelse(size[2] > 50, size[2] / dpi, size[2])
        } else {
            out$dir <- paste0(out$dir, a)
        }
    }

    if (out$filename == "")
        out$filename <- build_ggbash_filename(conf, out, argv[1])
    # FIXME multiple same aes (i.e. point x=Pt | smooth x=Age )

    out$filepath <- paste0(out$dir, "/", dataset_string, "/", out$filename)

    return(out)
}

#' save a ggplot object into a file
#'
#' @param dataset_string A character. Used as a directory.
#' @param ggstr A list of aesthetics
#' @param conf A list of aesthetics, non-aesthetics and geoms
#' @param argv A character vector
#'
#' @importFrom grDevices dev.off
#' @importFrom grDevices png
#' @importFrom grDevices pdf
save_ggplot <- function(
    dataset_string = "mtcars-32",
    ggstr = "ggplot(iris) + geom_point(aes(Sepal.Width, Sepal.Length))",
    conf = list(aes = c("x=cyl", "y=mpg"), non_aes = c(), geom_list = "point"),
    argv = c("png", "200*500", "'my-file-name'", "my_plot_dir")
){
    attrl <- parse_plot_attributes(argv, conf, dataset_string)
    dir.create(attrl$dir, showWarnings = FALSE)
    oldwd <- setwd(attrl$dir)
    dir.create(dataset_string, showWarnings = FALSE)
    setwd(dataset_string)
    setwd(oldwd)

    ggplot2::ggsave(attrl$filepath, plot = eval(parse(text = ggstr)),
                    width = attrl$w, height = attrl$h,
                    units = "in", dpi = attrl$dpi)
    message("saved: ", attrl$filepath)
}

remove_unnecessary_parentheses <- function(
    # FIXME parentheses for no equal case
    input = "gg(mtcars, x=factor(cyl), mpg) + text(label=paste0('label:', wt))"
){
    paren2space <- function(input, i)
        paste0(substr(input, 1, i - 1), " ",
               substr(input, i + 1, nchar(input)))
    n_paren <- 0
    is_after <- FALSE
    state <- ""
    for ( i in 1:nchar(input)) {
        this <- substr(input, i, i)
        if (is_after) {
            if (this == "(")
                n_paren <- n_paren + 1
            else if (this == ")")
                n_paren <- n_paren - 1

            # nested equals are ignored

            if (n_paren < 0) {
                is_after <- FALSE
                n_paren <- n_paren + 1
                input <- paren2space(input, i)
            }
        } else {
            if (this == "(")
                input <- paren2space(input, i)
            else if (this == ")")
                input <- paren2space(input, i)
            else if (this == "=")
                is_after <- TRUE
        }
        state <- paste0(state, n_paren)
    }
    return(input)
}

coat_adhoc_syntax_sugar <- function(
    cmd = "gg(mtcars,mpg,hwy) + point(size = xyz(gear) +1, shape = 16 / 3 * 4)"
){
    out <- gsub(",", " ", cmd) # no comma
    out <- gsub("\\s*=\\s*", "=", out)
    out <- gsub("\\s*\\+\\s*", "\\+", out)
    out <- gsub("\\s*\\*\\s*", "\\*", out)
    out <- gsub("\\s*-\\s*", "-", out)
    out <- gsub("\\s*/\\s*", "/", out)
    out <- remove_unnecessary_parentheses(out)
    return(out)
}

#' the core function of ggbash
#'
#' @param cmd A character
#'
#' compile_ggbash returns a built ggplot object.
#'
compile_ggbash <- function(cmd){
    cmd <- coat_adhoc_syntax_sugar(cmd)
    ggobj <- rly::yacc(Ggplot2Parser)$parse(
        cmd, rly::lex(Ggplot2Lexer)
    )
    return(ggobj)
}

#' execute raw ggbash commands
#'
#' @param raw_input A ggbash command chain (might contain pipes)
#' @param show_warn Whether to show a warning message
#'                    when ambiguously matched. Default is TRUE.
#' @param batch_mode Default is FALSE.
#'                  If TRUE, the resulted ggplot object is returned.
#' @param as_string Return the resulted ggplot2 object as a string
#'                  not as a ggplot2 object. Default is FALSE.
#'
#' @export
exec_ggbash <- function(raw_input="gg mtcars + point mpg cyl | copy",
                        show_warn=TRUE, batch_mode=FALSE,
                        as_string = FALSE){
    const <- define_ggbash_constants()
    commandv <- split_by_pipe(raw_input)
    ggobj <- ""
    for (cmd in commandv) {
        argv <- split_by_space(cmd)
        if (grepl(paste0("^", argv[1]), "ggplot2")) {
            dataset <- set_ggbash_dataset(argv[2])
            ggbashenv$colname <- colnames(dataset)
            if (show_warn)
                ggbashenv$show_amb_warn <- TRUE
            else
                ggbashenv$show_amb_warn <- FALSE
            # sometimes people input commas
            # due to daily habits
            ggobj <- compile_ggbash(cmd)
        } else if (argv[1] == "show") {
            print(tibble::as_data_frame(eval(as.symbol(argv[2]))))
            ggbashenv$colname <- colnames(eval(as.symbol(argv[2])))
            return(FALSE)
        } else if (argv[1] %in% c("echo", "print")) {
            if (ggobj != "")
                print(eval(parse(text = ggobj)))
            message(ifelse(ggobj != "", ggobj, argv[2]))
            return(FALSE)
        } else if (argv[1] %in% const$builtinv) {
            execute_ggbash_builtins(raw_input, argv, const)
        } else if (argv[1] %in% c("copy", "cp")) {
            copy_to_clipboard(ggobj)
        } else if (argv[1] %in% const$savev) {
            dataset_str <- paste0(attr(dataset, "ggbash_datasetname"),
                                  "-", nrow(dataset))
            save_ggplot(dataset_str, ggobj, ggbashenv$conf, argv)
        } else if (argv[1] %in% c("exit", "quit", "q")) {
                return(TRUE)
        } else {
            stop("unknown command is supplied")
        }
    }

    if (is.null(ggobj))
        return(FALSE) # ggobj is NULL when p_error() is called

    if (grepl(GGPLOT2INVALIDTOKEN, ggobj)) {
        message("\nThe built ggplot2 object is :\n  ",
                gsub("\\+ gg", "\\+ \n    gg",
                     gsub("ggplot2::", "", ggobj)))
        return(FALSE)
    }

    built_ggplot2_obj <- eval(parse(text = ggobj))

    if (batch_mode) {
        if (as_string)
            return(gsub("ggplot2::", "", ggobj))
        else
            return(built_ggplot2_obj)
    } else {
        print(built_ggplot2_obj)
    }

    return(FALSE)
}

#' Enter into a ggbash session.
#'
#' \code{ggbash_} executes a new ggbash session for faster ggplot2 plotting.
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
#' @param batch A character. If given, \code{ggbash_()} will exit
#'              just after executing the given command.
#' @param clipboard Default is NULL
#'                  If batch is non-empty and clipboard is non-NULL,
#'                  ggbash copies a resulted ggplot2 object to clipboard.
#'              just after executing the given command.
#' @param show_warn Whether to show a warning message
#'                    when ambiguously matched. Default is TRUE.
#' @param as_string Return the resulted ggplot2 object as a string
#'                  not as a ggplot2 object. Default is FALSE.
#'                  Ignored when non-batch mode.
#'
#' \describe{
#'     \item{Geom name:}{the geom most frequently used (based on my experiences)}
#'     \item{Column name:}{the column with the smallest column index}
#'     \item{Aesthetics:}{required (x, y), non-missing (shape, size), default (alpha, stroke) }
#' }
#' @return nothing
#'
#' @examples
#' \dontrun{ ggbash() # enter into an interactive ggbash session
#'
#' # plot a ggplot2 figure
#' ggbash_("gg iris + point Petal.Width Petal.Length")
#'
#' #' # plot a ggplot2 figure and copy the result
#' ggbash_("gg iris + point Petal.Width Petal.Length", 1)
#' }
#'
#' @export
ggbash_ <- function(batch="", clipboard=NULL,
                   show_warn=TRUE, as_string = FALSE) {
    if (batch != "") {
        raw_input <- batch
        if (! is.null(clipboard))
            raw_input <- ifelse(grepl(raw_input, "|\\s*copy"),
                                raw_input, paste0(raw_input, " | copy"))
        return(exec_ggbash(raw_input,
                           show_warn, batch_mode = TRUE,
                           as_string = as_string))
    }
    while (TRUE) {
        tryCatch({
            raw_input <- show_prompt()
            if (exec_ggbash(raw_input, show_warn))
                break
        },
          error = function(err) advice_on_error(err, raw_input), # by stop()
        finally = add_input_to_history(raw_input) # add even if failed
    )}
}

#' Tell ggbash to execute a given ggbash command
#'
#' \code{ggbash()} is used in the following three different ways:
#' 1. ggbash("gg mtcars + point mpg cyl") : with a character argument
#' 2. ggbash(gg(mtcars) + point(mpg,cyl)) : with a pseudo-ggplot2 command
#' 3. ggbash() : with no argument (enter into an interactive ggbash session)
#'
#' While ggbash can interpret "with- and without- parentheses" and
#' "with- and without- commas" commands in 1 and 3,
#' 2. can only interpret the with-parentheses and with-commas case
#' because of R's default token constraints.
#'
#' @param ggbash_symbols Non-evaluated R symbols or
#'                       a character representing ggbash commands.
#'                       If no ggbash_symbols are specified,
#'                       enter into an interactive ggbash session.
#' @param clipboard Default is NULL.
#'                  If non-null, copy the resulted string to clipboard.
#' @param show_warn If ambiguously matched, display warning. Default is TRUE.
#' @param as_string Return a string instead of a ggplot2 object.
#'                  Default is FALSE.
#'
#'
#' @export
ggbash <- function(ggbash_symbols="", clipboard=NULL,
                   show_warn=TRUE, as_string = FALSE) {
    is_string <- tryCatch(class(ggbash_symbols) == "character",
                            error = function(err) {FALSE})
    if (is_string) {
        cmd <- ggbash_symbols
    } else {
        raw_cmd <- deparse(substitute(ggbash_symbols),
                           width.cutoff = 500) # arbitrary large
        cmd <- raw_cmd
    }
    dbgmsg(cmd)
    return(ggbash_(cmd, clipboard = clipboard,
                   show_warn = show_warn, as_string = as_string))
}

#' print useful debug advice according to the given error message
#'
#' @param err_message A character returned by \code{stop}
#' @param raw_input A character given to \code{\link{ggbash}} function
#'
advice_on_error <- function(err_message,
                            raw_input="gg iris | p Sepal.W Sepal.L") {
    message(err_message)
    if (grepl("E001", err_message)) {
        # TODO list all data frame and matrices
    } else if (grepl("no such prefix", err_message)) {
        datasetname <- gsub("gg\\s([a-zA-Z0-9]+).*", "\\1", raw_input)
        message("  -- Did you give correct column names, geoms, or aesthetics?")
        show_dataset_column_indices(datasetname)
    }
}

#' retrieve required aesthetic names for a given geom
#'
#' @param suffix geom suffix
#'
#' @export
get_required_aes <- function(suffix="point") {
    command <- paste0("ggplot2::geom_", suffix, "()")
    expr <- parse(text = command)
    return(eval(expr)$geom$required_aes)
}

#' retrieve all aesthetic names for a given geom
#'
#' @param suffix geom suffix
#'
#' @export
get_possible_aes <- function(suffix="point") {
    command <- paste0("ggplot2::geom_", suffix, "()")
    expr <- parse(text = command)
    geom <- eval(expr)$geom
    possible_aesv <- unique(c(geom$required_aes,
                              geom$non_missing_aes,
                              names(geom$default_aes)))

    if(suffix == "bar") # FIXME adhoc
        possible_aesv <- c(possible_aesv, "weight")

    return(possible_aesv)
}

#' get geom parameters
#'
#' Some geoms like geom_text has special non-aes fields such as check_overlap.
#'
#' @param suffix geom suffix
#'
#' @seealso \code{\link{get_stat_params}}
#'
#' @examples
#'
#' \dontrun{ get_geom_params("point") }
#' # returns "na.rm"
#'
#' \dontrun{ get_geom_params("text") }
#' # returns "parse" "check_overlap" "na.rm"
#'
get_geom_params <- function(suffix="point") {
    if (suffix == "map") # FIXME
        return("")

    command <- paste0("ggplot2::geom_", suffix, "()")
    expr <- parse(text = command)
    geom_params <- eval(expr)$geom_params
    return(names(geom_params))
}

#' return stat params
#'
#' Some geoms such as \code{geom_smooth} or \code{geom_histogram}
#' often set stat parameters (\code{method="lm"} or \code{binwidth}).
#' The stat parameters is not stored in \code{geom_*()$geom_params},
#' cannot be obtained by \code{\link{get_geom_params}}
#' thus retrieve here by another procedure
#'
#' @param suffix geom suffix
#'
#' @seealso \code{\link{get_geom_params}}
#'
#' @examples
#'
#' \dontrun{ get_stat_params("histogram") }
#' # returns "binwidth" "bins" "na.rm" "pad"
#'
#' \dontrun{ get_stat_params("smooth") }
#' # returns "na.rm" "method" "formula" "se"
#'
#' \dontrun{ get_stat_params("violin") }
#' # returns "trim" "scale" "na.rm"
#'
#' \dontrun{
#' for (geom in define_ggbash_constants()$geom_namev)
#'     message(geom, " ", paste0(get_stat_params(geom), collapse=" "))
#' }
#'
#'
get_stat_params <- function(suffix="smooth") {
    if (suffix == "map") # FIXME
        return("")

    command <- paste0("ggplot2::geom_", suffix, "()")
    expr <- parse(text = command)
    stat_params <- names(eval(expr)$stat_params)
    # na.rm is duplicated within
    # geom_point()$geom_params and geom_point()$stat_params

    stat_list <- ls(pattern = "^stat_",
                    envir = asNamespace("ggplot2"))
    stat_sth  <- paste0("stat_", suffix)
    if (stat_sth %in% stat_list) {
        command <- paste0("ggplot2::stat_", suffix, "()")
        expr <- parse(text = command)
        stat_params <- c(stat_params,
                         names(eval(expr)$stat_params),
                         eval(expr)$stat$parameters(TRUE))
    }
    return(stat_params)
}

get_layer_params <- function(suffix="bin2d") {
    # FIXME should read layer.R
    specials <- get_geom_params(suffix)
    stats <- get_stat_params(suffix)
    wrappers <- c("stat", "position", "group", "show.legend")
    return(unique(c(specials, stats, wrappers)))
}

#' convert given ggbash strings into ggplot2 aesthetic specifications
#'
#' @param i An integer of index
#' @param aesv A vector of aesthetics
#' @param must_aesv A vector of required aesthetics
#' @param all_aesv A vector of possible aesthetics.
#' @param colnamev A vector of column names of a dataframe.
#' @param show_warn a flag for printing warning when ambiguous match.
#'                    Default is TRUE.
#'
#' must_aesv and all_aesv are built by
#' \code{\link{get_required_aes}} and
#' \code{\link{get_possible_aes}}, respectively.
#'
#' @export
parse_ggbash_aes <- function(i, aesv, must_aesv, all_aesv,
                             colnamev, show_warn=TRUE){
    # TODO as.factor as.character cut substr
    if (grepl("=", aesv[i])) {
        before_equal <- gsub("\\s*=.*", "", aesv[i])
    } else {
        # no aes specification like geom_point(aes(my_x, my_y))
        before_equal <- must_aesv[i]

        if (i > length(must_aesv))
            stop("too many unspecified aesthetics. ",
                 "Required aesthetics (in order) are: ",
                 paste0(must_aesv, collapse = ", "))
    }
    after_equal  <- gsub(".*=\\s*", "", aesv[i])

    if (substr(aesv[i],1,1) == "z") {
        # FIXME defaultZproblem - z should not be removed
        before_equal <- "z"
        # knowing "z" is needed for this geom is super hard...
        # must_aesv should contain "z" for geom_contour
        # but should not for geom_point...
    } else {
        if (! before_equal %in% all_aesv)
            before_equal <- all_aesv[find_first_index(before_equal, all_aesv, show_warn)]

    }

    if (grepl("group", before_equal))
        return(paste0(before_equal, "=", after_equal))

    # design decision: column name only by prefix match?
    aftr <- parse_after_equal(after_equal, colnamev, show_warn)
    if (is.null(aftr))
        return(NULL)

    return(paste0(before_equal, "=", aftr))
}

#'  convert given ggbash strings into ggplot2 non-aesthetic (constant) specifications
#'
#' @param non_aes A character of a non-aesthetic key and value pair
#' @param all_aesv A vector of possible aesthetics.
#' @param colnamev A character vector representing column names
#' @param show_warn a flag for printing warning when ambiguous match.
#'                    Default is TRUE.
#'
#' all_aesv are built by \code{\link{get_possible_aes}}.
#' \code{\link{parse_ggbash_aes}}
#'
parse_ggbash_non_aes <- function(non_aes="shape=1", all_aesv,
                                 colnamev, show_warn=TRUE){
    before_equal <- gsub("\\s*=.*", "", non_aes)
    after_equal  <- gsub(".*=\\s*", "", non_aes)

    if (! before_equal %in% all_aesv) # partial match
        before_equal <- all_aesv[find_first_index(before_equal, all_aesv, show_warn)]

    if (length(before_equal) == 0) # no such parameter
        return(NULL)

    after_equal <- parse_after_equal(after_equal, colnamev, show_warn)
    if (is.null(after_equal))
        return(NULL)

    return(paste0(before_equal, "=", after_equal))
}

#' parse symbols after equal sign
#'
#' x=factor(Sepal.W + 1) should be interpreted as x = factor(Sepal.Width + 1).
#'
#' @param after A string after equal sign.
#' @param colnamev A character vector representing column names.
#' @param show_warn Show warning message. Default is TRUE.
#'
#' @importFrom sourcetools tokenize_string
parse_after_equal <- function(
    after="1 + Sepal.W^2*3",
    colnamev = c("Sepal.Width", "Sepal.Length", "Species"), show_warn = TRUE
){
    info <- sourcetools::tokenize_string(after)
    nospace <- info[info$type != "whitespace", ]
    nospace$after <- c(nospace[-1, ]$value, "end")
    nospace$bval  <- c("start", nospace[-nrow(nospace), ]$value)# before value
    # TODO how can I know each symbol is a function?
    # especially func(first, second) arguments.
    not_call <- nospace$type == "symbol" & nospace$after != "("
    not_piped <- nospace$type == "symbol" & ! nospace$bval %in% c("%>%", "%T>%")
    not_special <- ! grepl("\\.\\..*\\.\\.", nospace$value)
    candidates <- nospace[not_call & not_piped & not_special, ]

    if (nrow(candidates) == 0)
        return(after)

    for ( i in 1:nrow(candidates)) {
        index <- find_first_by_prefix(candidates$value[i],
                                        colnamev, show_warn)
        if (is.null(index))
            return(NULL)
        candidates$value[i] <- colnamev[index]
    }

    info[as.numeric(rownames(candidates)), "value"] <- candidates$value

    # handyShortcuts
    info[info$value == "f", "value"] <- "factor"
    info[info$value == "n", "value"] <- "as.numeric"
    # might conflict with dplyr::n()?

    return(paste0(info$value, collapse=""))
}
