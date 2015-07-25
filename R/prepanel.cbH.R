#' @title Lattice panels for error bars and envelop bands
#' 
#' @name prepanel.cbH
#'
#' @description Used to do or add error bars or envelop bands in 
#' plots. It is used in with \code{\link{panel.cbH}}.
#' 
#' @param y central value (point estimate).
#'
#' @param ly lower limit.
#'
#' @param uy upper limit.
#'
#' @param subscripts see \code{\link[lattice]{panel.xyplot}}
#'
#' @return None is returned.
#'
#' @seealso \code{\link{panel.cbH}}
#'
#' @export
prepanel.cbH <- function(y, ly, uy, subscripts){
    ly <- as.numeric(ly[subscripts])
    uy <- as.numeric(uy[subscripts])
    y <- as.numeric(y[subscripts])
    list(ylim=range(y, uy, ly, finite=TRUE))
}
