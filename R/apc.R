#' @name apc
#' @author Walmes Zeviani, \email{walmes@@ufpr.br}.
#' @export
#' @title Generate Matrix of All Pairwise Comparisons (Tukey contrasts)
#' @description This function takes a matrix where each line defines a
#'     linear function of the parameters to estimate a marginal mean
#'     (aka least squares mean) and return the matrix that define the
#'     contrasts among these means. All pairwise contrasts are returned
#'     (aka Tukey contrasts). The matrix with these contrasts can be
#'     passed to \code{\link[multcomp]{glht}} to estimate them or used
#'     in explicit matricial calculus.
#' @param lfm a \eqn{k \times p} matrix where each line defines a linear
#'     function to estimate a lsmean. In general, these matrices are
#'     obtained by using \code{\link[doBy]{LSmatrix}}.
#' @param lev a character vector with length equals to the numbers of
#'     lines of \code{lfm} matrix, (\eqn{k}). Default is \code{NULL} and
#'     the row names of code{lfm} is used. If row names is also
#'     \code{NULL}, incremental integer values are used to identify the
#'     comparisons.
#' @return a \eqn{K\times p} matrix with the linear functions that
#'     define all pairwise contrasts. \eqn{K} is \eqn{{k}\choose{2}}.
#' @seealso \code{\link{apmc}}, \code{\link[doBy]{LSmatrix}}.
#' @examples
#'
#' X <- diag(3)
#' rownames(X)
#' apc(X)
#'
#' rownames(X) <- letters[nrow(X):1]
#' apc(X)
#'
#' apc(X, lev = LETTERS[1:nrow(X)])
#'
#' # Objects from doBy::LSmatrix() have an "grid" attribute.
#' attr(X, "grid") <- data.frame(n = LETTERS[1:nrow(X)])
#' rownames(X) <- NULL
#' apc(X)
#'
apc <- function(lfm, lev = NULL) {
    nlev <- nrow(lfm)
    rn <- rownames(lfm)
    a <- attr(lfm, "grid")
    if (is.null(lev)) {
        if (!is.null(a)) {
            lev <- apply(a, 1, paste, collapse = ":")
        } else if (!is.null(rn)) {
            lev <- rn
        } else {
            lev <- as.character(1:nlev)
        }
    }
    cbn <- combn(seq_along(lev), 2)
    M <- lfm[cbn[1, ], ] - lfm[cbn[2, ], ]
    if (is.vector(M)) {
        dim(M) <- c(1, length(M))
    }
    rownames(M) <- paste(lev[cbn[1, ]], lev[cbn[2, ]], sep = "-")
    return(M)
}
