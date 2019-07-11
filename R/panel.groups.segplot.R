#' @name centfac
#' @export
#' @author Walmes Zeviani, \email{walmes@@ufpr.br}.
#' @title Numeric Centered Factor Levels
#'
#' @description This function receveis a factor and return a numeric
#'     vector with equally spaced factor levels centered at 0.
#'
#' @param group A factor.
#' @param space A numeric value to be used as the space between
#'     levels. If \code{NULL}, the space is determined by the
#'     \code{group}.
#' @examples
#'
#' centfac(warpbreaks$tension)
#' centfac(warpbreaks$tension, space = 1)
#' centfac(warpbreaks$wool)
#' centfac(warpbreaks$wool, space = 1)
#'
centfac <- function(group, space = NULL) {
    stopifnot(is.factor(group))
    if (is.null(space)) {
        space <- 0.5/nlevels(group)
    }
    d <- 2 * ((as.integer(group) - 1)/(nlevels(group) - 1)) - 1
    return(space * d)
}

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
#'     \code{\link[latticeExtra]{segplot}()}.
#'
#' @param groups The grouping variable (\code{factor}), with the same
#'     length of \code{lwr} e \code{upr}.
#'
#' @param gap Scalar that is the distance among segments. Default is
#'     0.1. If the grouping factor has \eqn{k} levels, so \eqn{0 \leq
#'     \textrm{gap} < 1/k}. A negative value for \code{gap} will put the
#'     segments in a reversed order.
#'
#' @seealso \code{\link[latticeExtra]{segplot}()}.
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
#'         groups = tension, gap = 0.05,
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
    z <- as.numeric(z) + centfac(groups, space = gap)
    latticeExtra::panel.segplot(x, y, z, centers = centers,
                                subscripts = subscripts, ...)
}
