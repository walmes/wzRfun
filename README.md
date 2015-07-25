wzRfun
=============================================================================

> This is R. There is no if. Only how. Simon 'Yoda' Blomberg, R-help
> (April 2005)

This package contains functions that were developed over time for
analysis and representation of data in addition to other general-purpose
tasks. The package name has a very obvious composition except for the
fact that I prefer to think about Rfun as *R is fun* and not R
*functions to R*.

## Instalation

You can install this package from GitHub runnig the code below in a R session.

```{r}

## lib <- "/usr/lib/R/site-library"; repos <- "http://cran-r.c3sl.ufpr.br/"
## install.packages("devtools", dep=TRUE, repos=repos, lib=lib)
install.packages("devtools")
require(devtools)
install_github("walmes/wzRfun")

```

You can also install from a compressed file (`zip` or `tar.gz`). This
files are available for download at
http://www.leg.ufpr.br/~walmes/wzRfun/. Choose the proper file for your
operational system and prefer the last version.

RStudio users can install by menu:
  1. Tools > Install Packages...
  2. Install from: Package Archive File
  3. Locate the file already downloaded to complete installation.

You can also install by
```{r}
install.packages("/home/Downloads/wzRfun_0.6.tar.gz", repos=NULL)
```
Obviously, you must replace for the current path, version and extension
of the file that you downloaded.

For linux users, it is possible install by terminal by
```{sh}
R CMD INSTALL /home/Downloads/wzRfun_0.6.tar.gz
```

## Bug report

Please, leave your message at the
[issues field](https://github.com/walmes/wzRfun/issues). It will be
answered as fast as possible.
