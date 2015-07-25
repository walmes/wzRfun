require(roxygen2)

## getwd()
## setwd("../")
setwd("/home/walmes/GitHub/")

roxygenise(package.dir="wzRfun")

## system("R CMD check wzRfun")
## system("R CMD build wzRfun")

require(devtools)

setwd("/home/walmes/GitHub/wzRfun/")
file.remove(".#makePkg.R")
check()
build()

## require(wzRfun)
## packageVersion("wzRfun")
