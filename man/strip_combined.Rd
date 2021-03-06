% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/strip_combined.R
\name{strip_combined}
\alias{strip_combined}
\title{Generate a combined strip for lattice plots}
\source{
This function entirely based on a message in the R-help
    mailing list:
    \url{http://r.789695.n4.nabble.com/merging-plot-labels-in-a-lattice-plot-td2276609.html}.
}
\usage{
strip_combined(which.given, which.panel, factor.levels, var.name, ...)
}
\arguments{
\item{which.given, which.panel, factor.levels, var.name, ...}{See
\code{\link[lattice]{strip.custom}()}.}
}
\value{
A combined strip for trellis plot.
}
\description{
This function create a combined strip when two factor
    are used after the conditional operator em lattice functions
    (\code{y ~ x | A + B}).
}
\examples{

library(lattice)

da <- expand.grid(A = gl(2, 10), B = gl(3, 1))
da$x <- runif(nrow(da))
da$y <- rnorm(nrow(da), mean = da$x, sd = 1)

xyplot(y ~ x | A + B, data = da)

xyplot(y ~ x | A + B,
       data = da,
       strip = strip_combined,
       par.settings = list(layout.heights = list(strip = 0.5)))

}
\author{
Walmes Zeviani, \email{walmes@ufpr.br}.
}
