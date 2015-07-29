#' @title A panel to rotate and choose the better view angle of a
#' wireframe
#' 
#' @name rp.wire
#'
#' @description A panel rotate a wireframe plot and choose the best view
#' angle.
#' 
#' @param wire a wireframe object.
#'
#' @return Opens a window with sliders for \code{x}, \code{y} and
#' \code{z}. Print the \code{dput} os the last view angle using the
#' button.
#'
#' @seealso \code{\link[lattice]{wireframe}}, \code{\link[wzRfun]{panel.3d.contour}}.
#'
#' @export
#' @examples
#' \donttest{
#' ##-----------------------------------------------------------------------------
#' ## A simple example.
#' 
#' require(rpanel)
#' require(lattice)
#' require(latticeExtra)
#' require(RColorBrewer)
#' 
#' colr <- brewer.pal(11, "Spectral")
#' colr <- colorRampPalette(colr, space="rgb")
#' 
#' grid <- expand.grid(x=seq(-1, 1, by=0.1),
#'                     y=seq(-1, 1, by=0.1))
#' grid$z <- with(grid, 1+0.01*x+0.05*y-0.5*x*y-0.5*x^2-0.2*y^2)
#' 
#' p1 <- wireframe(z~x+y, data=grid,
#'                 scales=list(arrows=FALSE),
#'                 ## zlim=extendrange(grid$z, f=0.5),
#'                 ## panel.3d.wireframe=panel.3d.contour,
#'                 ## nlevels=18, col="gray30",
#'                 ## type=c("on", "top", "bottom"),
#'                 col.regions=colr(101),  drape=TRUE)
#' p1
#' 
#' 
#' ## Choose the better angle.
#' rp.wire(p1)
#'
#' }
rp.wire <- function(wire){
    if (!requireNamespace("rpanel", quietly=TRUE)) {
        stop("rpanel needed for this function to work. Please install it.",
             call.=FALSE)
    }
    draw.wire <- function(panel){
        sc <- list(x=panel$x, z=panel$z, y=panel$y)
        print(update(panel$wire, screen=sc))
        panel
    }
    print.deput <- function(panel){
        sc <- list(x=panel$x, z=panel$z, y=panel$y)
        ## print(dput(sc))
        dput(sc)
        panel
    }
    panel <- rp.control(wire=wire)
    rp.slider(panel, variable=x, from=-180, to=180, initval=-60,
              action=draw.wire, title="x", showvalue=TRUE, resolution=5)
    rp.slider(panel, variable=y, from=-180, to=180, initval=-20,
              action=draw.wire, title="y", showvalue=TRUE, resolution=5)
    rp.slider(panel, variable=z, from=-180, to=180, initval=-10,
              action=draw.wire, title="z", showvalue=TRUE, resolution=5)
    rp.button(panel, action=print.deput, title="dput screen values")
}
