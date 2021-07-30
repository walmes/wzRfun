#' @name open_in_gnumeric
#' @export
#' @author Walmes Zeviani, \email{walmes@@ufpr.br}.
#' @title Open data frames with Gnumeric
#' @description Takes a data frame, saves in a temporary file and opens
#'     it with Gnumeric spread sheet software. This function only works
#'     on Linux and requires Gnumeric installed. It can be modified to
#'     be used with another software and operating system. The code is
#'     based on this answer \url{https://stackoverflow.com/a/28580683}
#'     on StackOverflow.
#' @param tb A data frame.
#' @return None. This function opens the data frame with Gnumeric
#' @examples
#'
#' \dontrun{
#'
#' open_in_gnumeric(iris)
#' open_in_gnumeric(PlantGrowth)
#' open_in_gnumeric(swiss)
#'
#' }
#'
open_in_gnumeric <- function(tb) {
    stopifnot(inherits(tb, "data.frame"))
    tFile <- tempfile(fileext = paste0(substitute(tb), ".tsv"))
    utils::write.table(x = tb,
                       file = tFile,
                       row.names = FALSE,
                       sep = "\t",
                       quote = FALSE)
    stopifnot(Sys.info()["sysname"] == "Linux")
    u <- system("which gnumeric", intern = TRUE)
    cond <- length(u) > 0 && grepl("gnumeric", u)
    if (cond) {
        system(sprintf("gnumeric '%s'", tFile))
    } else {
        stop("Gnumeric was not found. Please install it.")
    }
    invisible()
}
