#' @export
#' @importFrom utils installed.packages packageDescription
#' @author Walmes Zeviani, \email{walmes@@ufpr.br}.
#' @title Returns the fields with packages in DESCRIPTION with versions
#' @description This function returns the packages mentioned in the
#'     DESCRIPTION file of a package and returns the current versions of
#'     them appended after the package name. So, you can copy and paste
#'     directly in the DESCRIPTION file to inform the minimal versions
#'     required for each dependence. The versions appended are those of
#'     the package in the current environment. So, be aware of using the
#'     correct packages version to develop packages and inform version
#'     only of those packages that are critical for the functionality of
#'     yours.
#' @param pkg \code{character[1]} Name of an available package.
#' @return When assigned, the return value is a character that can be
#'     properly display with \code{cat()}.
#' @examples
#'
#' \dontrun{
#'
#' pkgs_versions("lattice")
#'
#' devtools::load_all("~/Projects/wzRfun/")
#' pkgs_versions("wzRfun")
#'
#' devtools::load_all("~/Projects/labestData")
#' u <- pkgs_versions("labestData")
#' cat(u)
#'
#' }
pkgs_versions <- function(pkg) {
    ip <- installed.packages()
    append_version <- function(x) {
        sprintf("%s (>= %s)",
                x,
                if (x == "R") {
                    getRversion()
                } else {
                    # packageVersion(x)
                    ip[x, "Version"]
                })
    }
    append_pkg_version <- function(x) {
        sprintf("%s (>= %s)",
                x,
                # packageVersion(x)
                ip[x, "Version"])
    }
    dcp <- packageDescription(pkg)
    txt <- character()
    if (is.character(dcp$Depends)) {
        pkgs <- strsplit(dcp$Depends, ",[[:space:]]+")[[1]]
        pkgs <- sub(" .*", "", pkgs)
        pkgs <- sapply(pkgs, FUN = append_version)
        x <- c("Depends:\n    ",
               paste0(pkgs, collapse = ",\n    "),
               "\n")
        txt <- append(txt, x)
    }
    if (is.character(dcp$Imports)) {
        imp <- strsplit(dcp$Imports, ",[[:space:]]+")[[1]]
        pkgs <- sub(" .*", "", imp)
        pkgs <- sapply(pkgs, FUN = append_pkg_version)
        x <- c("Imports:\n    ",
               paste0(pkgs, collapse = ",\n    "),
               "\n")
        txt <- append(txt, x)
    }
    if (is.character(dcp$Suggests)) {
        imp <- strsplit(dcp$Suggests, ",[[:space:]]+")[[1]]
        pkgs <- sub(" .*", "", imp)
        pkgs <- sapply(pkgs, FUN = append_pkg_version)
        x <- c("Suggests:\n    ",
               paste0(pkgs, collapse = ",\n    "),
               "\n")
        txt <- append(txt, x)
    }
    cat(txt, sep = "")
    invisible(paste0(txt, collapse = ""))
}
