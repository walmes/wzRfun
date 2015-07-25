#' @title Lattice panel to non overlapping segments in \code{segplot()}
#' 
#' @name panel.segplotBy
#'
#' @description This panel allows no overlapping of segments in
#' \code{latticeExtra::segplot()}.
#'
#' @param x,y,centers,subscripts,... see \code{\link[latticeExtra]{segplot}}.
#' 
#' @param groups the grouping variable. Must be a factor.
#' 
#' @param f factor that is the vertical distance among arrows.
#'
#' @return None is returned.
#'
#' @seealso \code{\link[latticeExtra]{segplot}}
#'
#' @export
#' 
#' @examples
#' require(doBy)
#' require(multcomp)
#' require(plyr)
#' require(latticeExtra)
#' 
#' m0 <- lm(breaks~wool*tension, data=warpbreaks)
#' X <- LSmatrix(m0, effect=c("wool", "tension"))
#' attributes(X)
#' 
#' Xs <- by(X, INDICES=attr(X, "grid")$tension, FUN=as.matrix)
#' Xs <- lapply(Xs, "rownames<-", levels(warpbreaks$wool))
#' L <- lapply(Xs, apmc, model=m0, focus="wool", test="single-step",
#'             level=0.10)
#' 
#' L <- ldply(L)
#' names(L)[1] <- "tension"
#' L$tension <- factor(L$tension, levels=levels(warpbreaks$tension))
#' str(L)
#' 
#' segplot(wool~lwr+upr, centers=estimate, data=L, draw=FALSE)
#' 
#' segplot(tension~lwr+upr, centers=estimate, data=L, draw=FALSE,
#'         panel=panel.segplotBy, groups=L$wool, f=0.05)
panel.segplotBy <- function(x, y, z, centers, subscripts, groups, f, ...){
    d <- 2*((as.numeric(groups)-1)/(nlevels(groups)-1))-1
    z <- as.numeric(z)+f*d
    panel.segplot(x, y, z, centers=centers,
                  subscripts=subscripts, ...)
}
