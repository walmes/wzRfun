#' @title Round numeric columns of a table with desired number of digits
#' @name tableFormat
#'
#' @description This function returns a table with numeric columns
#' rounded accordingly the specified number of decimal digits. Numeric
#' columns are coverted to character after rounding.
#'
#' @param table a data frame.
#'
#' @param digits a vector with non negative integer values with lenght
#' equal the number of columns of \code{table}. These number represent
#' the number of digits used to round numeric values. The number is
#' ignored when the correponding columns aren't numeric.
#'
#' @return a table with column rounded to the number of decimal digits
#' specified. All columns are coverted to character, so numeric
#' functions can not be direct applied to them anymore. Because of this,
#' it is not recommended assign the result of the function to the object
#' used \code{table} argument.
#'
#' @seealso \code{\link{tableCap}}, \code{\link{matrix2html}}.
#' 
#' @examples
#' \donttest{
#' x <- tableFormat(head(rock), c(1,2,3,4)); x
#' str(x)
#' 
#' x <- tableFormat(head(iris), c(2,3,2,3,0)); x
#' str(x)
#'
#' tableFormat(head(rock), c(-1,NA,3,4))
#' tableFormat(as.matrix(head(rock)), c(1,2,3,4))
#' }
tableFormat <- function(table, digits){
    if(!is.data.frame(table)){
        stop("`table` must be a data.frame.")
    }
    if(ncol(table)!=length(digits)){
        stop("Length of `digits` is not equal the `table` number of columns.")
    }
    if(any(is.na(digits)) | any(digits<0)){
        stop("`digits` mustn't have missing (NA) or negative values.")
    }
    tb <- table
    for(i in 1:ncol(tb)){
        x <- tb[,i]
        d <- digits[i]
        if(is.numeric(x)){
            f <- paste0("%0.", d, "f")
            x <- sprintf(f, x) 
        } else {
            x <- as.character(x)
        }
        tb[,i] <- x
    }
    return(tb)
}
