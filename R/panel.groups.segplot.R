#' @name panel.groups.segplot
#' @export
#' @author Walmes Zeviani, \email{walmes@@ufpr.br}.
#' @title Panel to Plot Confidence Intervals by Groups in
#'     \code{segplot()}
#'
#' @description This function allows non overlapping error bars in
#'     \code{latticeExtra::segplot()}. It has the \code{groups =}
#'     argument.
#'
#' @param x,y,z,centers,data,subscripts,... Arguments passed to
#'     \code{\link[latticeExtra]{segplot}}.
#'
#' @param groups The grouping variable (\code{factor}), with the same
#'     length of \code{lwr} e \code{upr}.
#'
#' @param gap Scalar that is the distance among segments. Default is
#'     0.1. If the grouping factor has \eqn{k} levels, so \eqn{0 \leq
#'     \textrm{gap} < 1/k}. A negative value for \code{gap} will put the
#'     segments in a reversed order.
#'
#' @seealso \code{\link[latticeExtra]{segplot}}.
#' @import latticeExtra
#' @examples
#'
#' library(latticeExtra)
#'
#' m0 <- lm(log(breaks) ~ wool * tension, data = warpbreaks)
#'
#' pred <- with(warpbreaks, expand.grid(KEEP.OUT.ATTRS = TRUE,
#'                                      wool = levels(wool),
#'                                      tension = levels(tension)))
#'
#' pred <- cbind(pred,
#'               predict(m0, newdata = pred, interval = "confidence"))
#' str(pred)
#'
#' segplot(wool ~ lwr + upr, centers = fit, data = pred,
#'         draw = FALSE, horizontal = FALSE)
#'
#' segplot(wool ~ lwr + upr, centers = fit, data = pred,
#'         draw = FALSE, horizontal = FALSE,
#'         groups = tension, gap = NULL,
#'         panel = panel.groups.segplot)
#'
panel.groups.segplot <- function(x, y, z, centers,
                                 groups, gap = NULL,
                                 data, subscripts, ...) {
    if (!missing(data)) {
        data <- eval(data, envir = parent.frame())
        groups <- data[, deparse(substitute(groups))]
    }
    stopifnot(is.factor(groups))
    stopifnot(length(groups) == length(z))
    if (is.null(gap)) {
        gap <- 0.5/nlevels(groups)
    }
    d <- 2 * ((as.numeric(groups) - 1)/(nlevels(groups) - 1)) - 1
    z <- as.numeric(z) + gap * d
    panel.segplot(x, y, z, centers = centers,
                  subscripts = subscripts, ...)
}
