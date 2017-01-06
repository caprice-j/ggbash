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


#' define constant values used in ggbash
#'
#' \code{define_ggbash_constant_list} has no side effect.
#' It is similar with the 'const' modifier in C or C++.
#'
#' One thing to note is \code{define_ggbash_constant_list} set implicitly
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
define_ggbash_constant_list <- function(){
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
