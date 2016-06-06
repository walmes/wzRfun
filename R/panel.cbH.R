#' @name panel.cbH
#' @export
#' @author Walmes Zeviani, \email{walmes@@ufpr.br}, based on the R-help
#'     mail list.
#' @title Lattice panels for error bars and envelop bands
#' @description Used to plot confidence bars or confidence bands in
#'     lattice plots.
#' @param y central value (point estimate).
#' @param ly lower limit.
#' @param uy upper limit.
#' @param cty string that is the confidence type. Values current acepted
#'     are \code{"bars"} for error bars (confidence intervals) and
#'     \code{"bands"} for confidence/envelop bands.
#' @param desloc a numeric vetor with length equal to \code{y} used to
#'     desloc vertically the values to avoid overlapping with points or
#'     others factor levels intervals.
#' @param fill a color to fill the polygon defined by the confidence
#'     bands. Default is 1 that is the black color. Not used when
#'     \code{cty = "bars"}.
#' @param alpha transparecy level for the polygon defined by the
#'     confidence bands. Default is 0.1. Not used when \code{cty =
#'     "bars"}.
#' @param length is the length of the upper/lower error bars
#'     whiskers. Default is 0.05. Not used when \code{cty = "bands"}.
#' @param x,subscripts,col.line,lwd,... arguments passed to
#'     \code{\link[lattice]{xyplot}}.
#' @return None is returned.
#' @examples
#'
#' require(lattice)
#' require(latticeExtra)
#'
#' m0 <- lm(sqrt(dist)~speed, data=cars)
#' pred <- with(cars,
#'              data.frame(speed=seq(min(speed),
#'                             max(speed), length.out=20)))
#' aux <- predict(m0, newdata=pred, interval="confidence")
#' pred <- cbind(pred, aux)
#'
#' xyplot(sqrt(dist)~speed, data=cars)+
#'     as.layer(xyplot(fit~speed, data=pred, type="l",
#'                     ly=pred$lwr, uy=pred$upr, cty="bands",
#'                     fill="blue", alpha=0.3,
#'                     prepanel=prepanel.cbH,
#'                     panel=panel.cbH))
#'
#' m1 <- lm(weight~feed, data=chickwts)
#' pred <- with(chickwts,
#'              data.frame(feed=levels(feed)))
#' aux <- predict(m1, newdata=pred, interval="confidence")
#' pred <- cbind(pred, aux)
#'
#' xyplot(weight~feed, data=chickwts)+
#'     as.layer(xyplot(fit~feed, data=pred,
#'                     ly=pred$lwr, uy=pred$upr, cty="bars",
#'                     prepanel=prepanel.cbH,
#'                     desloc=rep(0.15, length(pred$fit)),
#'                     panel=panel.cbH))
#'
#' da <- expand.grid(trt=gl(2,1), x=1:7)
#' da$y <- with(da, as.integer(trt)+0.5*x+rnorm(x,0,0.4))
#' xyplot(y~x, groups=trt, data=da)
#'
#' m2 <- lm(y~trt+x, data=da)
#'
#' pred <- with(da,
#'              expand.grid(trt=levels(trt),
#'                          x=seq(min(x),
#'                              max(x), length.out=20)))
#' aux <- predict(m2, newdata=pred, interval="confidence")
#' pred <- cbind(pred, aux)
#'
#' xyplot(y~x, groups=trt, data=da)+
#'     as.layer(xyplot(fit~x, groups=trt, data=pred, type="l",
#'                     ly=pred$lwr, uy=pred$upr,
#'                     cty="bands", alpha=0.25,
#'                     prepanel=prepanel.cbH,
#'                     panel=panel.superpose,
#'                     panel.groups=panel.cbH))
panel.cbH <- function(x, y, ly, uy,
                      subscripts, cty,
                      col.line = plot.line$col,
                      lwd = plot.line$lwd,
                      desloc = NULL,
                      fill = 1, alpha = 0.1, length = 0.05, ...) {
    plot.line <- trellis.par.get("plot.line")
    if (is.null(desloc)) {
        desloc <- rep(0, length(uy))
    }
    y <- as.numeric(y)
    x <- as.numeric(x)
    or <- order(x)
    ly <- as.numeric(ly[subscripts])
    uy <- as.numeric(uy[subscripts])
    xo <- x[or]
    yo <- y[or]
    lyo <- ly[or]
    uyo <- uy[or]
    desl <- desloc[subscripts]
    if (cty == "bands") {
        panel.polygon(c(xo, rev(xo)), c(lyo, rev(uyo)), col = fill,
                      alpha = alpha, border = NA)
        panel.lines(xo, lyo, lty = 3, lwd = 0.5, col = col.line)
        panel.lines(xo, uyo, lty = 3, lwd = 0.5, col = col.line)
    }
    if (cty == "bars") {
        panel.arrows(xo + desl, lyo, xo + desl, uyo, length = length,
                     code = 3, angle = 90, col = col.line, lwd = lwd)
    }
    panel.xyplot(x + desl, y, subscripts = subscripts,
                 col.line = col.line, lwd = lwd, ...)
}

#' @name prepanel.cbH
#' @rdname panel.cbH
#' @export
prepanel.cbH <- function(y, ly, uy, subscripts){
    ly <- as.numeric(ly[subscripts])
    uy <- as.numeric(uy[subscripts])
    y <- as.numeric(y[subscripts])
    list(ylim=range(y, uy, ly, finite=TRUE))
}
