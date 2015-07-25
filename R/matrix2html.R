#' @title Convert a character matrix to HTML table
#' @name matrix2html
#'
#' @description This function covert a character matrix to html table.
#' 
#' @param x a character matrix. Is most of the cases it is the object
#' returned by \code{tableFormat()}.
#'
#' @param caption a string to be caption to the table. Default is
#' \code{NULL}.
#'
#' @param styDef a vector with style definition for \code{th}, \code{tr}
#' and \code{td} tags. See the examples section. Default is
#' \code{NULL}. These styles must be defined in a css file used to
#' render the html page. If you are using \code{knitr}, you can use a
#' custom css file when knitting
#' \code{knit2html("my_doc.Rmd", stylesheet="my_custom_css.css")}.
#'
#' @param styIndex a positive integer matrix with dimensions equal to
#' \code{x}. The numbers in each cell of \code{styIndex} call the
#' corresponding \code{styDef} to be used in \code{x}. See examples for
#' clarification. Default is \code{NULL}.
#'
#' @param tableClass a string corresponding a table style defined in a
#' css file. Default is \code{NULL}.
#'
#' @return a character vector. Use \code{cat()} inside chunks with
#' \code{results="asis"} to print the result to be interpreted by a
#' browser as an html table.
#'
#' @seealso \code{\link{tableFormat}}, \code{\link{tableCap}}.
#'
#' @export
#' @examples
#' \donttest{
#' x <- head(rock)
#' x <- sapply(x, as.character)
#' str(x)
#' m2h <- matrix2html(x)
#' cat(m2h)
#' 
#' x <- mtcars[1:10, 1:3]
#' x <- rbind(c("", colnames(x)), cbind(rownames(x), x))
#' x <- as.matrix(x)
#' rownames(x) <- colnames(x) <- NULL
#' x[1,1] <- "Cars"
#' x
#' 
#' m2h <- matrix2html(x, caption="Part of the cars data set.")
#' cat(m2h)
#' 
#' styIndex <- 0L*row(x)+3L
#' styIndex[3,] <- 4L
#' styIndex[4,] <- 5L
#' styIndex[5,] <- 6L
#' styIndex[7,] <- 7L
#' styIndex[8,] <- 8L
#' styIndex[9,] <- 9L
#' styIndex[,3] <- 3L
#' styIndex[1,] <- 1L
#' 
#' ## These table class are assumed to be defined in a css file as
#' ## follow.
#' 
#' ## td.red { color: #CC0000; }
#' ## td.blue { color: #0000CC; }
#' ## td.shade { background-color: #CCCCCC; }
#' ## td.line { border-bottom: 1px solid #000000; }
#' ## td.bold { font-weight: bold; }
#' ## td.italic { font-style: italic; }
#' 
#' styDef <- c("<th align=\"center\">%s</th>", "<tr>\n%s\n</tr>\n",
#'             "<td align=\"center\">%s</td>",
#'             "<td align=\"center\" class=\"red\">%s</td>",
#'             "<td align=\"center\" class=\"blue line\">%s</td>",
#'             "<td align=\"center\" class=\"shade\">%s</td>",
#'             "<td align=\"center\" class=\"line\">%s</td>",
#'             "<td align=\"center\" class=\"bold shade\">%s</td>",
#'             "<td align=\"center\" class=\"italic blue\">%s</td>")
#' 
#' m2h <- matrix2html(x=x, styDef=styDef, styIndex=styIndex,
#'                    caption="Part of the cars data set.")
#' cat(m2h)
#' }
matrix2html <- function(x, caption=NULL, styDef=NULL, styIndex=NULL,
                        tableClass=NULL){
    applysty <- function(x, sty, which=1L){
        sprintf(sty[which], x)
    }
    applySty <- Vectorize(applysty, c("x", "which"))
    if(is.null(styDef)){
        styDef <- c("<th>%s</th>", "<tr>\n%s\n</tr>\n", "<td>%s</td>")
    }
    if(is.null(styIndex)){
        styIndex <- 0L*row(x)+3L
        styIndex[1,] <- 1L
    }
    if(!(is.matrix(styIndex) & is.integer(styIndex))){
        stop("styIndex must be a integer matrix.")
    }
    if(!(length(styDef)>=3)){
        stop("styDef must have minimum length equal 3.")
    }
    if(!is.character(styDef)){
        stop("styDef must be character.")
    }
    if(!all(grepl("%s", styDef))){
        stop("All styDef elements must have %s.")
    }
    th <- grepl("^<th.*th>", styDef)[1]
    td <- grepl("^<td.*td>", styDef)[1]
    if(!(th | td)){
        stop("styDef[1] must be a definition for <th><\th> or <td></td>.")
    }
    tr <- grepl("^<tr.*tr>", styDef)[2]
    if(!tr){
        stop("styDef[2] must be a definition for <tr><\tr>.")
    }
    td <- grepl("^<td.*td>", styDef)[3]
    if(!td){
        stop("styDef[3] must be a definition for <td><\td>.")
    }
    if(nrow(x)!=nrow(styIndex) | ncol(x)!=ncol(styIndex)){
        stop("x and styIndex must be of the same dimension.")
    }
    if(max(styIndex)>length(styDef)){
        stop("There is some index outside the styDef provided.")
    }
    if(is.matrix(x) & is.character(x)){
        A <- applySty(unlist(x), styDef, which=unlist(styIndex))
        dim(A) <- dim(x)
        A <- apply(A, 1, paste, collapse="\n")
        A <- applysty(A, sty=styDef, which=2L)
    } else {
        stop("x must be a character matrix.")
        cat("Row names and column names must be the first column and row of this matrix.")
    }
    if(!is.null(caption)){
        cap <- c("<figcaption class=\"tab\">",
                 caption, "\n</figcaption>\n")
        A <- c(cap, A)
    }
    if(length(tableClass)==1){
        table <- sprintf("<table class=\"%s\">\n", tableClass)
    } else {
        table <- "<table>\n"
    }
    A <- c(table, A, "</table>\n")
    return(A)
}
