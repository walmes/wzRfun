#' @name equallevels
#' @export
#' @author Walmes Zeviani, \email{walmes@ufpr.br}.
#' @title Make Two Data Frames Have The Same Factor Levels
#' @description If two data frames have factors ou characters columns
#'     with the same column names, those in the first will have the same
#'     level order as those in the second. So, for factors, these data
#'     frames will have the same levels in the same order. Characters in
#'     the first will be converted to factor.
#' @param x the target data frame.
#' @param y the reference data frame.
#' @return the restructured data frame.
#' @examples
#'
#' a <- data.frame(Species = as.character(
#'                     sample(iris$Species, size = 20, repl = TRUE)),
#'                 stringsAsFactors = FALSE)
#' str(a)
#'
#' b <- equallevels(a, iris)
#' str(b)
#'
equallevels <- function(x, y) {
    if (is.data.frame(x) & is.data.frame(y)) {
        com <- intersect(x = names(x), y = names(y))
        for (i in com) {
            if (!is.null(levels(y[[i]]))) {
                x[[i]] <- factor(x[[i]], levels = levels(y[[i]]))
            }
        }
        return(x)
    } else {
        stop("`x` and `y` must be a data.frame.")
    }
}
