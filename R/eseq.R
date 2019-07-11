#' @name eseq
#' @export
#' @author Walmes Zeviani, \email{walmes@@ufpr.br}.
#' @title Extended regular sequence
#' @description This function returns an extended regular sequence based
#'     on a numeric vector. It is used mainly to create sequences of
#'     regression variables to predict the response of a model.
#' @param x A numeric vector used to generate the extended range
#'     sequence.
#' @param length.out An integer value that is the number of elements in
#'     the sequence, passed to the internal call of the
#'     \code{\link[base]{seq}()} function.
#' @param f The expansion factor relative to the range of values in the
#'     vector \code{x}. It is passed to the internal call of the
#'     \code{\link[grDevices]{extendrange}()}.
#' @return A numeric vector of length \code{length.out} that is a new
#'     sequence based on \code{x}.
#' @examples
#'
#' eseq(x = 0:1, length.out = 11, f = 0.05)
#'
eseq <- function(x, length.out = 25, f = 0.05) {
    er <- grDevices::extendrange(x, f = f[1])
    return(seq(from = er[1],
               to = er[2],
               length.out = length.out))
}
