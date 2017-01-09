
#' return resulted strings of approximate string match
#'
#' @param fuzzy_input A character. Typically user input.
#' @param possibilities A character vector one of which
#'                      is assumed to be pointed by fuzzy_input.
#' @param n_top An integer specifying the number of returned strings.
#' @param case_sensitive Default is FALSE.
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
#' @export
get_analogue <- function(fuzzy_input = "axs.txt",
                         possibilities = c("axis.text", "axis.text.x"),
                         n_top = 5, case_sensitive = FALSE) {

    edit_distance_matrix <- adist(fuzzy_input, possibilities,
                                  ignore.case = case_sensitive)

    indices <- sort.int(edit_distance_matrix, index.return = TRUE)$ix[1:n_top]
    similar_strings <- possibilities[indices]

    return(similar_strings[! is.na(similar_strings)])
}
