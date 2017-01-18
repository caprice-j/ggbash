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
partial_unique <- function(
    originalv=c("mpg", "cyl", "disp", "hp", "drat"), i = 1) {

    out <- rep("", length(originalv))
    while (any(out == "")) {
        shortened <- sapply(originalv, function(s) substr(s, 1, i))
        dup_namev <- names(table(shortened))[table(shortened) > 1]
        index <- (! shortened %in% dup_namev) & (out == "")
        out[index] <- shortened[index]
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
#' find_first_by_prefix does a prefix partial matching.
#' ggbash tries to interpolate user's input by one of the following mechanism:
#' prefix match, partial match, or precedence-based guessing.
#'
#' @param prefix A prefix to be searched
#' @param table A character vector (typically aesthetic name list)
#' @param show_warn Show warning if matched ambiguously. Default is TRUE.
#'
#'
#' @return An integer representing index
#'
#' @export
find_first_by_prefix <-
    function(prefix="si",
             table=c("x", "y", "size", "shape"),
             show_warn=TRUE){
    if (prefix %in% table)
        # when exact match, return it
        # (Among "price" and "p" by pattern "p", return "p")
        return(which(prefix == table))

    indices <- grep(paste0("^", prefix), table)

    if (length(indices) < 1 && show_warn) {
        if (grepl("colo", prefix))
            indices <- grep(paste0("^colour"), table)
        else
            return(NULL)
            # stop 'no such prefix: '
    }
    if (length(indices) > 1 && show_warn &&
        (! prefix %in% c(sapply(1:5, function(i) substr("point", 1, i)),
                         sapply(1:4, function(i) substr("line",  1, i))))) {
        message("  WARNING: Ambiguous match. Use \"",
                table[indices][1], "\"\n",
                "           among ", paste0(table[indices], collapse = ", ")
                )
    }
    return(indices[1])
}

find_first_index <- function(
    pattern = "sz",
    table = c("x", "y", "size", "shape", "colour", "fill", "alpha", "stroke"),
    show_warn = TRUE
){
    first_char <- substr(pattern,1,1)
    # defaultZproblem
    if (first_char == "z")
        return(pattern)

    # handyShortcuts
    if (pattern == "a")
        # without this if statement,
        # "a" matches "stat" not "alpha"
        return(which(table == "alpha"))

    matched_df <- get_analogue(pattern, table)
    best_matched <- matched_df[1, ]
    return(best_matched$index)
}

#' define constant values used in ggbash
#'
#' \code{define_ggbash_constants} has no side effect.
#' It is similar with the 'const' modifier in C or C++.
#'
#' One thing to note is \code{define_ggbash_constants} set implicitly
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
#'          when doing partial match in GgplotParser.
#'
define_ggbash_constants <- function(){
    list(
        first_wd = getwd(),
        # BUILTIN command Vectors
        # Note: the following commands are not included -- see exec_ggbash
        #       echo print quit exit
        builtinv = c("cd", "dir", "dir.create", "ls", "list",
                     "mkdir", "pwd", "rm", "rmdir", "setwd"),
        # all geom in ggplot2 documents
        # the order in geom_namev is important
        # because build_ggplot_object() uses
        # the first element after partial matching
        # i.e. the preferable (frequently-used) geom should appear first
        geom_namev = c("abline", "area",
                       "bar", "bin2d", "blank", "boxplot",
                       "count", "curve", "contour", "crossbar",
                       "density", "density_2d", "dotplot",
                       "errorbar", "errorbarh",
                       "freqpoly", "histogram", "hline", "hex", "jitter",
                       # "l" matches "line" (the 1st element starting by "l")
                       "line", "label", "linerange",
                       "map",
                       # "p" matches to "point"
                       "point", "path", "polygon", "pointrange",
                       "quantile",
                       "rect", "rug", "raster", "ribbon",
                       "segment", "smooth", "step",
                       "text", "tile",
                       "vline", "violin"
        ),
        savev = c("png", "pdf"),
        themedf = get_all_theme_aes()
        # TODO implement stat like stat_smooth
    )
}

get_element_tree_clone <- function() {

    # devtools::check() add a note about using ggplot2:::.element_tree
    # because it is an internal object of other packages.
    # Thus, a quick-and-dirty solution,
    # I just copied the resulted data frame here.
    #
    # This is done for ggplot2 2.2.1 (commit 464e0f3) on January 6, 2017.

    rect_data <-
        matrix(strsplit(
        "line                                   line element_line
        rect                                   rect element_rect
        text                                   text element_text
        axis.title                       axis.title element_text
        axis.title.x                   axis.title.x element_text
        axis.title.x.top           axis.title.x.top element_text
        axis.title.y                   axis.title.y element_text
        axis.title.y.right       axis.title.y.right element_text
        axis.text                         axis.text element_text
        axis.text.x                     axis.text.x element_text
        axis.text.x.top             axis.text.x.top element_text
        axis.text.y                     axis.text.y element_text
        axis.text.y.right         axis.text.y.right element_text
        axis.ticks                       axis.ticks element_line
        axis.ticks.length         axis.ticks.length         unit
        axis.line                         axis.line element_line
        axis.line.x                     axis.line.x element_line
        axis.line.y                     axis.line.y element_line
        legend.background         legend.background element_rect
        legend.margin                 legend.margin       margin
        legend.spacing               legend.spacing         unit
        legend.spacing.x           legend.spacing.x         unit
        legend.spacing.y           legend.spacing.y         unit
        legend.key                     element_rect element_rect
        legend.key.size             legend.key.size         unit
        legend.key.height         legend.key.height         unit
        legend.key.width           legend.key.width         unit
        legend.text                     legend.text element_text
        legend.text.align         legend.text.align    character
        legend.title                   legend.title element_text
        legend.title.align       legend.title.align    character
        legend.position             legend.position    character
        legend.direction           legend.direction    character
        legend.justification   legend.justification    character
        legend.box                       legend.box    character
        legend.box.margin         legend.box.margin       margin
        legend.box.background          element_rect element_rect
        legend.box.spacing       legend.box.spacing         unit
        panel.background           panel.background element_rect
        panel.border                   element_rect element_rect
        panel.spacing                 panel.spacing         unit
        panel.spacing.x             panel.spacing.x         unit
        panel.spacing.y             panel.spacing.y         unit
        panel.grid.major               element_line element_line
        panel.grid.minor               element_line element_line
        panel.ontop                     panel.ontop      logical
        plot.background             plot.background element_rect
        plot.title                       plot.title element_text
        plot.subtitle                 plot.subtitle element_text
        plot.caption                   plot.caption element_text
        plot.margin                     plot.margin       margin
        strip.background           strip.background element_rect
        strip.placement             strip.placement    character
        strip.text                       strip.text element_text
        strip.text.x                   strip.text.x element_text
        strip.text.y                   strip.text.y element_text
        strip.switch.pad.grid strip.switch.pad.grid         unit
        strip.switch.pad.wrap strip.switch.pad.wrap         unit",
        "\\s+")[[1]], nrow = 3)

    aes_info <- as.data.frame(t(rect_data), stringsAsFactors = FALSE)
    colnames(aes_info) <- c("name", "unknown", "class")
    return(aes_info)
}

get_all_theme_aes <- function() {

    aes_info <- get_element_tree_clone()

    # Now all of aes_info$class is non-NULL and not element_blank().
    return(aes_info)
}

get_theme_elem_name_conf <- function(class = "element_text") {
    blacklist <- c("inherit.blank", "debug")

    if (class == "element_text")
        fields <- names(ggplot2::element_text())
    else if (class == "element_rect")
        fields <- names(ggplot2::element_rect())
    else if (class == "element_line")
        fields <- names(ggplot2::element_rect())

    conf <- fields[! fields %in% blacklist]
    return(conf)
}
