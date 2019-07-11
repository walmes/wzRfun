.onAttach <- function(lib, pkg) {
    if (getOption("wzRfun_startup_message", TRUE)) {
        pkg_info <- drop(read.dcf(
            file = system.file("DESCRIPTION", package = "wzRfun"),
            fields = c("Package", "Title", "Version",
                       "Date", "URL", "BugReports")
        ))
        dashes <- paste0(rep("----------", times = 7), collapse = "")
        line1 <- sprintf("  %s: %s\n\n",
                         pkg_info["Package"],
                         pkg_info["Title"])
        line2 <- sprintf("  %s (%s%s) was loaded.\n",
                         pkg_info["Package"],
                         pkg_info["Version"],
                         ifelse(is.na(pkg_info["Date"]),
                                yes = "",
                                no = sprintf(", built on %s",
                                             pkg_info["Date"])))
        line3 <- ifelse(!is.na(pkg_info["URL"]),
                        yes = sprintf("  Consult online documentation at:\n    %s\n",
                                      pkg_info["URL"]),
                        no = NA)
        line4 <- sprintf("  For collaboration, support or bug report, %s\n",
                         ifelse(is.na(pkg_info["BugReports"]),
                                yes = "contact the maintainer.",
                                no = paste0("visit:\n    ",
                                            pkg_info["BugReports"])))
        line5 <- sprintf("  For general information: packageDescription(\"%s\")\n",
                         pkg_info["Package"])
        line6 <- sprintf("  To access the package documentation: help(package = \"%s\")\n",
                         pkg_info["Package"])
        packageStartupMessage(paste0(dashes, "\n",
                                     line1,
                                     line2,
                                     if (!is.na(line3)) line3,
                                     line4,
                                     line5,
                                     line6,
                                     dashes))
    }
}
