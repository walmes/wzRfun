#' @title Coeficient of determination for non linear regression models
#' @name R2nls
#'
#' @description This function returns the anova table with the partition
#' of the total sum of squares into due the regression model and
#' residuals and the corresponding \eqn{R^2} (adjusted for the null
#' model).
#'
#' @param nls.obj a object of class \code{nls}.
#'
#' @return a list with two slots. The first named \code{anova} is the
#' table with partition of the total sum of squares (adjusted for the
#' mean or null model or intercept inly model) in regression sum of
#' squares and residual sum of squares. The second named \code{R2} is
#' the coefficient of determination.
#'
#' @export
#' @examples
#' \donttest{
#' da <- subset(DNase, Run==4)
#' plot(density~log(conc), data=da)
#' 
#' m0 <- nls(density~Asym/(1+exp(-(log(conc)-xmid)/scal)),
#'           start=list(Asym=2.3, xmid=1.5, scal=1),
#'           data=da)
#' summary(m0)
#' 
#' plot(density~log(conc), data=da)
#' with(as.list(coef(m0)),
#'      curve(Asym/(1+exp(-(x-xmid)/scal)),
#'            add=TRUE, col=2))
#' 
#' R2nls(m0)
#' cor(fitted(m0), da$density)^2
#' }
R2nls <- function(nls.obj){
    if(class(nls.obj)!="nls")
        stop("The object must be of class `nls`.")
    da <- eval(nls.obj$data)
    resp.name <- all.vars(summary(nls.obj)$formula)[1]
    form <- paste(resp.name, "~1", sep="")
    m0 <- lm(form, da)
    an <- anova(nls.obj, m0)
    sqn <- deviance(nls.obj)
    sqe <- deviance(m0)
    r2 <- 1-(sqn/sqe)
    aov <- data.frame(fv=c("regression","residuals"),
                      gl=c(-an$Df[2],an$Res.Df[1]),
                      sq=c(-an$Sum[2],an$Res.Sum[1]))
    aov$qm <- aov$sq/aov$gl
    aov$F <- c(aov$qm[1]/aov$qm[2], NA)
    aov$"Pr(>F)" <- c(1-pf(aov$F[1], df1=aov$gl[1], df2=aov$gl[2]), NA)
    names(aov) <- c(" ","Df", "Sum Sq", "Mean Sq", "F value", "Pr(>F)")
    return(list(anova=aov, R2=r2))
}
