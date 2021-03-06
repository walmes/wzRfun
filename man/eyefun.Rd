% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eyefun.R
\name{eyefun}
\alias{eyefun}
\title{GUI to Minipulate Non Linear Regression Models With Sliders}
\usage{
eyefun(model, start, dots = NULL)
}
\arguments{
\item{model}{a formula with, at least, the right hand side.}

\item{start}{a list in many elements as number of parameters in
\code{model}. Each list element must have a parameter name. All
elements must be numeric vector of length 2 or 3 (\code{from},
\code{init} and \code{to}) used to create the sliders with
\pkg{rpanel}.}

\item{dots}{a named list with arguments passed to the
\code{\link[graphics]{curve}()}.}
}
\value{
A GUI with one slider for each parameter in \code{model} to
    manipulate the curve of the model.
}
\description{
This function creates a GUI to manipulate parameters of
    a model with sliders. It is useful to get insight of a model.
}
\examples{

\dontrun{

library(rpanel)

model <- y ~ A/(1 + exp(-(x - M)/S))
start <- list(A = c(2, 5),
              M = c(0, 5),
              S = c(-1, 0.5, 1))
eyefun(model, start,
       dots = list(lwd = 2, col = "blue",
                   from = 0, to = 10,
                   ylim = c(0, 5),
                   xlab = "Age", ylab = "Weight",
                   main = expression(alpha/(1 + exp(-(x - M)/S)))))

}
}
\author{
Walmes Zeviani, \email{walmes@ufpr.br}, based on suggestion
    in the R-br mailing list (Brazil),
    \href{http://r-br.2285057.n4.nabble.com/R-br-transferir-modelo-da-funcao-nls-para-a-funcao-curve-automaticamente-td4665756.html}{\emph{[R-br]
    transferir modelo da função nls para a função curve()
    automaticamente}}.
}
