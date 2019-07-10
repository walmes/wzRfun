#' @name strip_combined
#' @author Walmes Zeviani, \email{walmes@@ufpr.br}.
#' @export
#' @title Generate a combined strip for lattice plots
#' @description This function create a combined strip when two factor
#'     are used after the conditional operator em lattice functions
#'     (\code{y ~ x | A + B}).
#' @param which.given,which.panel,factor.levels,var.name,... See
#'     \code{\link[lattice]{strip.custom}()}.
#' @return A combined strip for trellis plot.
#' @source This function entirely based on a message in the R-help
#'     mailing list:
#'     \url{http://r.789695.n4.nabble.com/merging-plot-labels-in-a-lattice-plot-td2276609.html}.
#' @examples
#'
#' library(lattice)
#'
#' da <- expand.grid(A = gl(2, 10), B = gl(3, 1))
#' da$x <- runif(nrow(da))
#' da$y <- rnorm(nrow(da), mean = da$x, sd = 1)
#'
#' xyplot(y ~ x | A + B, data = da)
#'
#' xyplot(y ~ x | A + B,
#'        data = da,
#'        strip = strip_combined,
#'        par.settings = list(layout.heights = list(strip = 0.5)))
#'
strip_combined <- function(which.given,
                           which.panel,
                           factor.levels,
                           var.name,
                           ...) {
    print(var.name)
    if (which.given == 1) {
        panel.rect(0, 0, 1, 1, col = "grey90", border = 1)
        panel.text(x = 1,
                   y = 0.5,
                   pos = 2,
                   lab = paste0(var.name[1], ": ",
                                factor.levels[which.panel[which.given]]))
    }
    if (which.given == 2) {
        panel.text(x = 0, y = 0.5, pos = 4,
                   lab = paste0(var.name[2], ": ",
                                factor.levels[which.panel[which.given]]))
    }
}
