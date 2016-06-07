#' @name table_caption
#' @export
#' @author Walmes Zeviani, \email{walmes@@ufpr.br}.
#' @title Numbered Caption Labels For Exported Tables
#' @description This function does numered caption for exported tables.
#' @param caption a string that is the caption for a table.
#' @param reset integer value used to reset the table caption
#'     counter. Default is \code{NULL}.
#' @param label a string representing the name for table. Default is
#'     \code{Tabela}.
#' @param sep a string with the separator. Default is \code{:}.
#' @return a string composed by \code{label}, \code{sep} and
#'     \code{caption}. Each call increments its number.
#' @seealso \code{\link{tableFormat}}, \code{\link{matrix2html}}.
#' @examples
#'
#' table_caption("First table label.")
#' table_caption("Second table label.")
#' table_caption("Third table label.")
#' table_caption("Counter reseted.", reset = 0)
#'
table_caption <- function(caption,
                          reset = NULL,
                          label = "Tabela",
                          sep = ":") {
    if (!exists(".tabEnv", mode = "environment", envir = .GlobalEnv)) {
        .tabEnv <<- new.env(parent = .GlobalEnv)
        assign("n", 0, envir = .tabEnv)
    }
    if (!is.null(reset)) {
        assign("n", reset, envir = .tabEnv)
    } else {
        assign("n", .tabEnv$n + 1L, envir = .tabEnv)
    }
    sprintf("%s %d%s %s", label, .tabEnv$n, sep, caption)
}

#' @name table_format
#' @export
#' @author Walmes Zeviani, \email{walmes@@ufpr.br}.
#' @title Round Numeric Columns of a Table
#' @description This function returns a table with numeric columns
#'     rounded accordingly the specified number of decimal
#'     digits. Numeric columns are coverted to character after rounding.
#' @param table a data frame.
#' @param digits a vector with non negative integer values with length
#'     equals the number of columns of \code{table}. These number
#'     represent the number of digits used to round numeric values. The
#'     number is ignored when the correponding column isn't
#'     numeric. \code{NA} and negative values are ignored.
#' @return a table with column rounded to the number of decimal digits
#'     specified. All columns are coverted to character, so numeric
#'     functions can not be direct applied to them anymore. Because of
#'     this, it is not recommended assign the result of the function to
#'     the object used as \code{table} argument.
#' @seealso \code{\link{table_caption}}, \code{\link{matrix2html}}.
#' @examples
#'
#' x <- table_format(head(rock), digits = c(1, 2, 3, 4))
#' x
#' str(x)
#'
#' x <- table_format(head(iris), c(2, 3, 2, 3, NA))
#' x
#' str(x)
#'
#' x <- table_format(head(rock), c(-1, NA, 3, 4))
#' x
#' str(x)
#'
table_format <- function(table, digits) {
    if (!is.data.frame(table)) {
        stop("`table` must be a data.frame.")
    }
    if (ncol(table) != length(digits)) {
        stop(paste0("Length of `digits` is not equal ",
                    "the `table` number of columns."))
    }
    tb <- table
    for (i in which(digits >= 0)) {
        x <- tb[, i]
        d <- digits[i]
        if (is.numeric(x)) {
            f <- paste0("%0.", d, "f")
            x <- sprintf(f, x)
        } else {
            x <- as.character(x)
        }
        tb[, i] <- x
    }
    return(tb)
}

#' @name matrix2html
#' @export
#' @author Walmes Zeviani, \email{walmes@@ufpr.br}.
#' @title Convert a character matrix to HTML table
#' @description This function coverts a character matrix to an html
#'     table.
#' @param x a character matrix. Is most of the cases it is the object
#'     returned by \code{table_format()}.
#' @param caption a string to be the caption of the table. Default is
#'     \code{NULL}, no caption.
#' @param styles a vector with style definition for \code{th}, \code{tr}
#'     and \code{td} tags. See the examples section. Default is
#'     \code{NULL}. These styles must be defined in a css file used to
#'     render the html page. If you are using \code{knitr}, you can use
#'     a custom css file when knitting \code{knit2html("my_doc.Rmd",
#'     stylesheet = "my_custom_css.css")}.
#' @param indexes a positive integer matrix with dimensions equal to
#'     \code{x}. The numbers in each cell of \code{indexes} call the
#'     corresponding \code{styles} to be used in \code{x}. See examples
#'     for clarification. Default is \code{NULL}.
#' @param class a string corresponding a table style defined in a
#'     \code{css} file. Default is \code{NULL}.
#' @return a character vector. Use \code{cat} inside chunks with header
#'     \code{results="asis"} to print the result as interpretable code
#'     of an html table.
#' @seealso \code{\link{table_format}}, \code{\link{table_caption}}.
#' @examples
#'
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
