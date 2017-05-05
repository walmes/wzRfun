wzRfun
=============================================================================

> This is R. There is no if. Only how. Simon 'Yoda' Blomberg, R-help
> (April 2005)

[![Build Status](https://travis-ci.org/walmes/wzRfun.svg?branch=master)](https://travis-ci.org/walmes/wzRfun)
Build status for the stable version (`master` branch)

This package contains functions that were developed for analysis and
representation of data in addition to other general-purpose tasks.  The
package name has a very obvious motivation except for the fact that I
prefer to think about `Rfun` as *R is fun* and not *R functions*.

## Instalation

You can install this package from GitHub runnig the code below in a R
session.

```{r}
library(devtools)
install_github("walmes/wzRfun", ref = "master")
```

You can also install from a compressed file (`zip` or `tar.gz`). This
files are available for download at
<http://leg.ufpr.br/~walmes/pacotes/>. Choose the proper file for your
operational system and prefer the last version.

RStudio users can install by menu:
  1. Tools > Install Packages...
  2. Install from: Package Archive File
  3. Locate the file already downloaded to complete installation.

You can also install by
```{r}
install.packages("wzRfun_0.6.tar.gz", repos = NULL, type = "source")
```

Obviously, you must replace for the current path, version and extension
of the file that you downloaded.

The code below uses XML queries (package XML) to download and install
the latest version.

```{r}
# Use "tar.gz" or "zip" for file extension.
sulfix <- "tar.gz"

library(XML)

# Gets urls of files.
urlp <- "http://www.leg.ufpr.br/~walmes/pacotes/"
links <- getHTMLLinks(urlp)
ptn <- sprintf("^wzRfun_(.*)\\.%s$", sulfix)
vers <- gsub(pattern = ptn,
             replacement = "\\1",
             x = grep(x = links,
                      pattern = ptn,
                      value = TRUE))

# Finds out the latest version.
if (length(vers) > 1) {
    cpv <- Vectorize(FUN = compareVersion)
    out <- outer(vers, vers, FUN = cpv)
    ver <- vers[which.max(rowSums(out))]
    pkglink <- sprintf("wzRfun_%s.%s", ver, sulfix)
} else {
    pkglink <- sprintf("wzRfun_%s.%s", vers, sulfix)
}

# Downloads in a temporary folder.
td <- tempdir()
pkgpath <- paste(td, pkglink, sep = "/")
download.file(url = paste0(urlp, pkglink),
              destfile = pkgpath)

# Installs the package from the compressed file.
install.packages(pkgpath, repos = NULL, type = "source")
```

For linux users, it is possible install by using the Linux terminal by
```{sh}
R CMD INSTALL wzRfun_0.6.tar.gz
```

## Bug report

Please, leave your message at the
[issues field](https://github.com/walmes/wzRfun/issues). It will be
answered as fast as possible.
