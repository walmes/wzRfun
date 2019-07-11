# @author Walmes Zeviani, \email{walmes@@ufpr.br}.
# @details This function is based on
#     \url{http://stackoverflow.com/questions/13365732/sorting-non-alphanumeric-characters-in-ascii-order-in-r}.
#     Any modification done were to fit my needs.
ascii_sort <- function(vec) {
    x <- sapply(vec,
                FUN = function(X) {
                    paste0(strtoi(x = charToRaw(X),
                                  base = 16),
                           collapse = "")
                })
    vec[order(x)]
}
# ascii_sort(vec = c(" 23", "_23", "123", "abc", "ABC"))

# @author Walmes Zeviani, \email{walmes@@ufpr.br}.
tree_files <- function(path = "./") {
    # Lista os arquivos e diretórios.
    lf <- list.files(path = path,
                     full.names = FALSE,
                     recursive = TRUE,
                     include.dirs = TRUE)
    # Ordena de acordo com o valor ascii.
    lf <- ascii_sort(lf)
    # Lista dos diretórios.
    ld <- list.dirs(path = path,
                    full.names = FALSE,
                    recursive = TRUE)
    # Quais são os diretórios.
    isdir <- lf %in% ld
    lf <- paste0("/", lf)
    # Quebra um caminho nas barras.
    x <- strsplit(x = lf, split = "/")
    # Substitui cada termo por dois espaços.
    tr <- sapply(x,
                 FUN = function(x) {
                     n <- length(x)
                     x <- paste0(paste(rep("    ", n - 1),
                                       collapse = ""),
                                 x[n])
                     gsub(x = x,
                          pattern = "^ {4}",
                          replacement = "")
                 })
    # Adiciona um barra atrás do nome de diretórios.
    tr[isdir] <- paste0(tr[isdir], "/")
    # Visual sem traços e pipes. A indentação é a hierarquia.
    cat(tr, sep = "\n")
    invisible(tr)
}

#' @export
#' @author Walmes Zeviani, \email{walmes@@ufpr.br}.
#' @title Show directories as indented names
#' @param path character[1] Is the root directory. Default is the value
#'     of \code{getwd()}.
#' @param options character[1] String of options to be passed to
#'     \code{tree} shell command. Default is
#'     \code{"-F --charset=ascii"}. See the available options with
#'     \code{system("tree --help")}.
#' @param linux logical[1] Default is \code{TRUE} and indicates that
#'     \code{tree} of the Linux SO is to be used. Otherwise, the R
#'     native approach will be used.
#' @return Prints an ASCII art of the directory tree.
#' @details This function calls
#'     \href{http://mama.indstate.edu/users/ice/tree/}{\code{tree}} if
#'     it is available is Linux OS. The call is made with
#'     \code{system()}. Otherwise, a (less sophisticated) R native
#'     approach is used.
#' @seealso \code{link{dir_tree}} for an implementation entirely based
#'     on R and functional in all SO. Also, \code{\link[base]{dir}},
#'     \code{\link[base]{list.files}} and \code{\link[base]{list.dirs}}
#'     can be useful in many circumstances.
#' @examples
#'
#' \dontrun{
#'
#' tree()
#' tree("../")
#' tree("../", linux = FALSE)
#'
#' system("tree --help")
#'
#' tree(system.file(package = "lattice"),
#'      options = "-F --charset=ascii -L 1")
#' tree(system.file(package = "lattice"),
#'      options = "-F --charset=ascii -L 2")
#'
#' }
#'
tree <- function(path = ".",
                 options = "-F --charset=ascii",
                 linux = TRUE) {
    if (Sys.info()["sysname"] == "Linux" && linux) {
        wis <- system("whereis tree", intern = TRUE)
        wis <- length(unlist(strsplit(wis, split = " "))) > 1
        if (wis) {
            system(sprintf("tree %s %s", path, options))
        } else {
            tree_files(path = path)
        }
    } else {
        tree_files(path = path)
    }
}
