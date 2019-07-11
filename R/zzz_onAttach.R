.onAttach <- function(lib, pkg) {
    if (getOption("wzRfun_startup_message", TRUE)) {
        pkg_info <- drop(read.dcf(
            file = system.file("DESCRIPTION", package = "wzRfun"),
            fields = c("Package", "Title", "Version", "Date", "URL")
        ))
        dashes <- paste0(rep("----------", times = 7), collapse = "")
        line1 <- sprintf("%s: %s",
                         pkg_info["Package"],
                         pkg_info["Title"])
        line2 <- sprintf("%s (%s%s) was loaded.",
                         pkg_info["Package"],
                         pkg_info["Version"],
                         ifelse(is.na(pkg_info["Date"]),
                                yes = "",
                                no = sprintf(", built on %s",
                                             pkg_info["Date"])))
        line3 <- sprintf("For collaboration, support or bug report, %s",
                         ifelse(is.na(pkg_info["URL"]),
                                yes = "contact the maintainer.",
                                no = paste0("visit:\n    ", pkg_info["URL"])))
        line4 <- sprintf("For general information: packageDescription(\"%s\")",
                         pkg_info["Package"])
        line5 <- sprintf("To access the package documentation: help(package = \"%s\")",
                         pkg_info["Package"])
        packageStartupMessage(paste0(dashes, "\n  ",
                                     line1, "\n\n  ",
                                     line2, "\n  ",
                                     line3, "\n  ",
                                     line4, "\n  ",
                                     line5, "\n",
                                     dashes))
    }
}
