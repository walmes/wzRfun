require(roxygen2)

getwd()
setwd("../")

roxygenise(package.dir="wzRfun")

system("R CMD check wzRfun")
system("R CMD build wzRfun")
