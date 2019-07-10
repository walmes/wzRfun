.onAttach <- function (lib, pkg) {
    startup <- getOption("wzRfun_startup_message", TRUE)
    if (startup) {
        pkg.info <- drop(read.dcf(
            file = system.file("DESCRIPTION", package = "wzRfun"),
            fields = c("Package", "Title", "Version", "Date", "URL")
        ))
        dashes <- paste0(rep("----------", times = 7), collapse = "")
        packageStartupMessage(
            paste0(
                dashes, "\n  ",
                pkg.info["Package"], ": ",
                pkg.info["Title"], "\n\n  ",
                "For collaboration, support or bug report, visit:\n    ",
                sub(x = pkg.info["URL"],
                    pattern = "\n",
                    replacement = "\n    "), "\n\n  ",
                pkg.info["Package"],
                " version ",
                pkg.info["Version"],
                " (build in ", pkg.info["Date"],
                ") was loaded.\n",
                dashes)
        )
    }
}
