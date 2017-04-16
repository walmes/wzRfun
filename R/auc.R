#' @name auc
#' @export
#' @author Walmes Zeviani, \email{walmes@@ufpr.br}.
#' @title Area under curve by trapezoidal integration
#' @description Calculates the area under curve based on the trapezoidal
#'     integration method.
#' @param time A vector with the time numeric variable.
#' @param resp A vector with the response numeric variable.
#' @return Returns the area under the curve based on the trapezoidal
#'     method.
#' @examples
#'
#' x <- 1:5
#' y <- c(1, 2, 3, 2, 1)
#'
#' plot(y ~ x,
#'      type = "o",
#'      ylim = c(0, max(y)),
#'      asp = 1)
#' polygon(x = c(x, rev(x)),
#'         y = c(y, 0 * y),
#'         border = NA,
#'         col = "purple")
#' abline(v = x, h = x, lty = 2, col = "gray")
#'
#' # Area for the colored region.
#' auc(time = x, resp = y)
#'
auc <- function(time, resp) {
    i <- !is.na(resp)
    time <- time[i]
    resp <- resp[i]
    alt <- diff(time)
    bas <- resp[-length(resp)] + diff(resp)/2
    a <- sum(alt * bas)
    return(a)
}
