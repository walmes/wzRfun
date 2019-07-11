wzRfun
========================================================================

[![Build Status](https://travis-ci.org/walmes/wzRfun.svg?branch=master)](https://travis-ci.org/walmes/wzRfun)
Build status for the stable version (`master` branch)

> This is R. There is no if. Only how.
>
> -- Simon 'Yoda' Blomberg, R-help (April 2005)

---

**Table of Contents**

  1. [Presentation](#presentation)
  2. [Instructions for installation](#instructions-for-installation)
  3. [Load only some functions](#load-only-some-functions)
  4. [Bug report](#bug-report)
  5. [Packages that uses `wzRfun`](#packages-that-uses-wzrfun)

## Presentation

`wzRfun` contains functions developed for analysis and representation of
data in addition to other general purpose tasks.  The package name has a
very obvious motivation except for the fact that I prefer to think about
`Rfun` as being *R is fun* and not being *R functions*.

You can visit the documentation of the package at:

  * Package webpage: <http://leg.ufpr.br/~walmes/pacotes/wzRfun/>. All
    public functions are documented at
    [Reference](http://leg.ufpr.br/~walmes/pacotes/wzRfun/reference/).
  * Package repository on GitHub:
    <https://github.com/walmes/wzRfun>. There you can see the source
    code, report bugs, and `source()` the code of functions without
    install the package.

## Instructions for installation

You can install `wzRfun` from GitHub running the code below in a R
session.

```r
library(devtools)
install_github(repo = "walmes/wzRfun", ref = "master")
```

You can also install it from a compressed file (`zip` or
`tar.gz`). These files are available for download at
<http://leg.ufpr.br/~walmes/pacotes/>. Choose the proper file for your
operational system and prefer the last version.

RStudio users can install it using the menu:

  1. Tools > Install Packages...
  2. Install from: Package Archive File
  3. Locate the file already downloaded to complete installation.
  4. If the installation fails, try:
     1. Upgrade the version of R for compatibility with `wzRfun`.
     2. Upgrade the version of the packages on which `wzRfun` depends.

You can also install `wzRfun` from compressed files by running the code
below.

```r
install.packages("wzRfun_X.Y.tar.gz", repos = NULL, type = "source")
```

Obviously, you must replace for the current path, version and extension
by those of of the file that you downloaded.

For convenience and reproducibility, the code below uses XML queries
(package `XML`) to download and install the latest version of `wzRfun`
available in the webpage directory of the package.

```r
# Use "tar.gz" or "zip" for file extension.
ext <- c("tar.gz", "zip")[1]

library(XML)

# Gets urls of files.
urlp <- "http://www.leg.ufpr.br/~walmes/pacotes/"
links <- getHTMLLinks(urlp)
ptn <- sprintf("^wzRfun_(.*)\\.%s$", ext)
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
    pkglink <- sprintf("wzRfun_%s.%s", ver, ext)
} else {
    pkglink <- sprintf("wzRfun_%s.%s", vers, ext)
}

# Downloads in a temporary directory.
td <- tempdir()
pkgpath <- paste(td, pkglink, sep = "/")
download.file(url = paste0(urlp, pkglink), destfile = pkgpath)

# Installs the package from the compressed file.
install.packages(pkgpath, repos = NULL, type = "source")
```

For GNU Linux users, it is possible install by using the Linux terminal
by

```sh
R CMD INSTALL wzRfun_X.Y.tar.gz
```

## Load only some functions

If you i) don't want to install the package, ii) are having problems
with installation or iii) only wants to use a function or two, you can
`source()` the desired code to your R session. You just need the URL for
the raw file with the code you want. For example, to get only
`rp.nls()`, you can execute as follows.

```r
source("https://raw.githubusercontent.com/walmes/wzRfun/master/R/rp.nls.R")
ls()
```

After that, `rp.nls()` will be an object available to you. Consult the
online documentation on
[`rp.nls()`](http://leg.ufpr.br/~walmes/pacotes/wzRfun/reference/rp.nls.html)
to learn how to use it.

## Bug report

Please, leave your message at the
[issues field](https://github.com/walmes/wzRfun/issues). It will be
answered as fast as possible.

## Packages that uses `wzRfun`

  * [EACS](http://leg.ufpr.br/~walmes/pacotes/EACS/).
  * [RDASC](http://leg.ufpr.br/~walmes/pacotes/RDASC/).
  * [nematistics](https://gitlab.c3sl.ufpr.br/walmes/nematistics).
