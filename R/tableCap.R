#' @title Incremental numbered caption labels
#' @name tableCap
#'
#' @description This funciton was made to export data frames to html
#' tables with knitr havind numered captions.
#' 
#' @param caption a string that is the caption for a table.
#'
#' @param resetTo integer value used to reset the table caption
#' counter. Default is \code{NULL}.
#'
#' @param captionName a string representing the name for table. Default
#' is \code{Tabela}.
#'
#' @param captionSep a string with the separator. Default is \code{:}.
#'
#' @return a string composed by \code{captionName}, \code{captionSep}
#' and \code{caption}. Each call increments its number.
#'
#' @seealso \code{\link{tableFormat}}, \code{\link{matrix2html}}.
#' 
#' @examples
#' \donttest{
#' tableCap("First table label.")
#' tableCap("Second table label.")
#' tableCap("Third table label.")
#' tableCap("Counter reseted.", resetTo=0)
#' }
tableCap <- function(caption, resetTo=NULL, captionName="Tabela",
                     captionSep=":"){
    if(!exists(".tabEnv", mode="environment", envir=.GlobalEnv)){
        .tabEnv <<- new.env(parent=.GlobalEnv)
        assign("n", 0, envir=.tabEnv)
    }
    if(!is.null(resetTo)){
        assign("n", resetTo, envir=.tabEnv)
    } else {
        assign("n", .tabEnv$n+1L, envir=.tabEnv)
    }
    sprintf("%s %d%s %s", captionName, .tabEnv$n, captionSep, caption)
}
