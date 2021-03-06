% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eseq.R
\name{eseq}
\alias{eseq}
\title{Extended regular sequence}
\usage{
eseq(x, length.out = 25, f = 0.05)
}
\arguments{
\item{x}{A numeric vector used to generate the extended range
sequence.}

\item{length.out}{An integer value that is the number of elements in
the sequence, passed to the internal call of the
\code{\link[base]{seq}()} function.}

\item{f}{The expansion factor relative to the range of values in the
vector \code{x}. It is passed to the internal call of the
\code{\link[grDevices]{extendrange}()}.}
}
\value{
A numeric vector of length \code{length.out} that is a new
    sequence based on \code{x}.
}
\description{
This function returns an extended regular sequence based
    on a numeric vector. It is used mainly to create sequences of
    regression variables to predict the response of a model.
}
\examples{

eseq(x = 0:1, length.out = 11, f = 0.05)

}
\author{
Walmes Zeviani, \email{walmes@ufpr.br}.
}
