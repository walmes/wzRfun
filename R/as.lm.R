#' @export
#' @aliases as.lm
#' @rdname as.lm
as.lm <- function(object, ...) UseMethod("as.lm")

#' @name as.lm
#' @aliases as.lm.nls
#' @export
#' @author Walmes Zeviani, \email{walmes@@ufr.br}
#' @title Converts nls class objects to lm class
#' @description This functions is a modified version of the code
#'     available at the \code{nls2} package on github
#'     \url{https://github.com/ggrothendieck/nls2}. This function is no
#'     longer available in the current version of \code{nls2} package.
#' @param object An object of class \code{nls}.
#' @param ... Currently not used.
#' @return This function returns an object of class \code{lm} created
#'     from the \code{nls} object. The linear model design matrix used
#'     is the the partial derivatives of the \code{nls} model function
#'     with relation to the model parameters, i.e. the gradient matrix.
#' @details This function is useful to get the residuals plot for the
#'     fitted nls model. The dependence on the \code{as.proto.list}
#'     function in the \code{proto} package was removed (also this
#'     function no longer exists). Thanks for the original author of
#'     this function G. Grothendieck.
#' @examples
#'
#' # An simple nls fit.
#' n0 <- nls(dist ~ A + B * speed^C,
#'           data = cars,
#'           start = c(A = 0, B = 1, C = 1))
#' summary(n0)
#'
#' # The results.
#' plot(dist ~ speed, data = cars)
#' with(as.list(coef(n0)), {
#'     curve(A + B * speed^C, xname = "speed", add = TRUE)
#' })
#'
#' # Residual analysis.
#' par(mfrow = c(2, 2))
#' plot(as.lm(n0))
#' layout(1)
#'
# if object is an "nls" object then its often used like this:
# predict(as.lm(object), ...) where ... are any predict.lm args
#
# as.lm.nls effectively just does this:
# lm(lhs ~ gradient - 1, offset = fitted(object),
#   list(gradient = object$m$gradient(), lhs = object$m$lhs()))
# so most of the code is just to get the names right.
#
as.lm.nls <- function(object, ...) {
    if (!inherits(object, "nls")) {
        w <- paste(
            "expected object of class nls but got object of class:",
            paste(class(object), collapse = " "))
        warning(w)
    }

    gradient <- object$m$gradient()
    if (is.null(colnames(gradient))) {
        colnames(gradient) <- names(object$m$getPars())
    }

    response.name <- if (length(stats::formula(object)) == 2) {
                         "0"
                     } else {
                         as.character(stats::formula(object)[[2]])
                     }
    lhs <- object$m$lhs()

    L <- data.frame(lhs, gradient)
    names(L)[1] <- response.name

    fo <- sprintf("%s ~ %s - 1",
                  response.name,
                  paste(colnames(gradient), collapse = " + "))
    fo <- stats::as.formula(fo)

    m <- do.call("lm",
                 list(fo,
                      offset = substitute(fitted(object)),
                      data = L))
    return(m)
}
