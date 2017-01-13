
#' return resulted strings of approximate string match
#'
#' @param fuzzy_input A character. Typically user input.
#' @param possibilities A character vector one of which
#'                      is assumed to be pointed by fuzzy_input.
#' @param n_top An integer specifying the number of returned strings.
#' @param case_sensitive Default is FALSE.
#' @param cost A named vector
#' @param threshold If costs are more than threshold,
#'                  remove them from the result
#'                  even if they are within top \code{n_top}.
#'                  Default is 6.
#'
#' \code{get_analogue} is a key function
#' for returning useful compile error message.
#'
#' @examples
#'
#' \dontrun{
#' get_analogue("axis.txt", c("axis.text", "axis.text.x", "axis.ticks"))
#' }
#' # returns "axis.text" "axis.text.x" "axis.ticks"
#'
#' @importFrom utils adist
#' @importFrom stats na.omit
#' @export
get_analogue <- function(fuzzy_input = "axs.txt",
                         possibilities = c("axis.text", "axis.text.x"),
                         n_top = 5, case_sensitive = FALSE,
                         cost = c(
                             "insertions" = .25,
                             "deletions" = 3,
                             "substitutions" = 2
                         ),
                         threshold = 6) {

    if (length(possibilities) == 0)
        return(NULL)

    edit_distance_matrix <- adist(fuzzy_input, possibilities,
                                  ignore.case = case_sensitive,
                                  costs = cost)
    sorted <- sort.int(edit_distance_matrix, index.return = TRUE)
    indices <- sorted$ix[1:n_top]
    with_NA <-
        data.frame(name = possibilities[indices],
                   cost = sorted$x[1:n_top],
                   nchar = nchar(possibilities[indices]),
                   index = indices,
                   stringsAsFactors = FALSE)
    # if tie, prefer longer string
    # (prefer "axis.text.x" than "axis.text")
    with_NA <- with(with_NA, with_NA[order(cost, -nchar), ])
    similar_string_df <- na.omit(with_NA)

    return(similar_string_df[similar_string_df$cost < threshold, ])
}
