#' @title Make that to data frames have the same factor levels
#' @name equallevels
#'
#' @description If two data frames have factor ou character columns with
#' the same name, those in the first will have the same level order as
#' those in the second. So, in terms of factor columns, these data
#' frames will have the same levels in the same order. Character columns
#' in the first will be converted to factor.
#' 
#' @param x the target data frame.
#'
#' @param y the reference data frame.
#'
#' @return the restructred data frame.
#'
#' @examples
#' \donttest{
#' a <- data.frame(Species=as.character(
#'                     sample(iris$Species, 10, repl=TRUE)),
#'                 stringsAsFactors=FALSE)
#' str(a)
#' b <- equallevels(a, iris)
#' str(b)
#' }
equallevels <- function(x, y){
    if(is.data.frame(x) & is.data.frame(y)){
        com <- intersect(names(x), names(y))
        for(i in com){
            if(!is.null(levels(y[,i]))){
                x[,i] <- factor(x[,i], levels=levels(y[,i]))
            }
        }
        x
    }
    else stop("`x` and `y` must be a data.frame.")
}
