#' @name panel.3d.contour
#' @export
#' @author Walmes Zeviani, \email{walmes@@ufpr.br}, based on the
#'     messages to the R-help mailing list.
#' @title Lattice Panel for Draw Contour Lines In Wireframe Plots
#' @description Used to superpose contour lines at the bottom, top or on
#'     the surface of a wireframe.
#' @param type a string vector. The values allowed are \code{top},
#'     \code{bottom} and/or \code{on}. Can be use more than one.
#' @param levels the same argument defined in
#'     \code{\link[lattice]{wireframe}}.
#' @param nlevels the same argument defined in
#'     \code{\link[lattice]{wireframe}}.
#' @param col.contour color of the contour lines.
#' @param x,y,z,rot.mat,distance,zlim.scaled,zlim,... arguments passed
#'     to panel.3dwire, the core function of wireframe.
#' @seealso \code{\link[lattice]{wireframe}}.
#' @examples
#'
#' library(lattice)
#' library(latticeExtra)
#' library(RColorBrewer)
#'
#' #--------------------------------------------
#' # Example 1.
#'
#' # display.brewer.all()
#' colr <- brewer.pal(11, "RdYlGn")
#' colr <- colorRampPalette(colr, space = "rgb")
#'
#' grid <- expand.grid(x = seq(0, 1, by = 0.05),
#'                     y = seq(0, 1, by = 0.05))
#' grid$z <- with(grid, x + y)
#'
#' wireframe(z ~ x + y, data = grid,
#'           scales = list(arrows = FALSE),
#'           panel.3d.wireframe = panel.3d.contour,
#'           levels = seq(0.5, 1.5, by = 0.1), type = "on",
#'           col.regions = colr(101), drape = TRUE)
#'
#' #--------------------------------------------
#' # Example 2.
#'
#' colr <- brewer.pal(11, "Spectral")
#' colr <- colorRampPalette(colr, space = "rgb")
#'
#' grid <- expand.grid(x = seq(-1, 1, by = 0.1),
#'                     y = seq(-1, 1, by = 0.1))
#' grid$z <- with(grid, 1 + 0.01 * x + 0.05 * y -
#'                      0.5 * x * y - 0.5 * x^2 - 0.2 * y^2)
#'
#' wireframe(z ~ x + y, data = grid,
#'           scales = list(arrows = FALSE),
#'           zlim = extendrange(grid$z, f = 0.5),
#'           panel.3d.wireframe = panel.3d.contour,
#'           nlevels = 18, col = "gray30",
#'           type = c("on", "top", "bottom"),
#'           col.regions = colr(101), drape = TRUE)
#'
panel.3d.contour <- function(x, y, z, rot.mat, distance,
                             type = c("on", "top", "bottom"),
                             nlevels = 20, levels = NULL, zlim.scaled,
                             zlim, col.contour = 1, ...) {
    # Test for the values at the argument `type=`.
    if (!any(type %in% c("on", "top", "bottom"))) {
        stop("`type=` must be \"on\", \"top\" and/or \"bottom\".")
    }
    if (is.null(levels)) {
        levels.scaled <- pretty(range(z, na.rm = TRUE), nlevels)
    } else {
        nlevels <- length(levels)
        levels.scaled <- (levels - zlim[1])/diff(range(zlim)) - 0.5
    }
    # Estimate the contour lines.
    clines <- contourLines(x, y,
                           matrix(z, nrow = length(x), byrow = TRUE),
                           nlevels = nlevels, levels = levels.scaled)
    # Draw contour lines on the floor (bottom).
    if (any(type %in% c("bottom"))) {
        for (ll in clines) {
            n <- ltransform3dto3d(rbind(ll$x, ll$y, zlim.scaled[1]),
                                  rot.mat, distance)
            panel.lines(n[1, ], n[2, ], col = col.contour,
                        lty = 1, lwd = 1)
        }
    }
    # Draw the wireframe surface.
    panel.3dwire(x, y, z, rot.mat, distance,
                 zlim.scaled = zlim.scaled, ...)
    # Draw contour lines on the surface (on).
    if (any(type %in% c("on"))) {
        for (ll in clines) {
            n <- ltransform3dto3d(rbind(ll$x, ll$y, ll$level),
                                  rot.mat, distance)
            panel.lines(n[1, ], n[2, ], col = col.contour,
                        lty = 1, lwd = 1)
        }
    }
    # Draw contour lines on the top of cube (top).
    if (any(type %in% c("top"))) {
        for (ll in clines) {
            n <- ltransform3dto3d(rbind(ll$x, ll$y, zlim.scaled[2]),
                                  rot.mat, distance)
            panel.lines(n[1, ], n[2, ], col = col.contour,
                        lty = 1, lwd = 1)
        }
    }
}
