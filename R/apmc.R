#' @title All pairwise mean comparisons
#' @name apmc
#'
#' @description This function performs all pairwise compararisons among
#' means returning pontual and intervalar estimates followed by letters
#' to easy discriminate values.
#'
#' @param X a matrix where each line is a linear function of the model
#' parameters to estimate a least squares mean. In most pratical cases,
#' it is an object from the \code{doBy::LSmatrix()}.
#'
#' @param model a model recognized by glht().
#'
#' @param focus name of the factor which levels are being compared.
#'
#' @param test a p-value correction method.
#'
#' @param level the experimentwise level of significance for the
#' multiple comparisons. The individual coverage of the confidence
#' interval is \code{1-level}.
#'
#' @return a \code{data.frame} with interval estimates and compact
#' letter display for the means comparisons.
#' 
#' @seealso \code{\link{apc}}, \code{\link[doBy]{LSmatrix}},
#' \code{\link[multcomp]{glht}}.
#'
#' @export
#' @examples
#' \donttest{
#' require(doBy)
#' require(multcomp)
#' ## source("apc.R")
#' 
#' ## Single factor.
#' m0 <- lm(weight~feed, data=chickwts)
#' anova(m0)
#' 
#' X <- LSmatrix(m0, effect="feed")
#' rownames(X) <- levels(chickwts$feed)
#' apmc(X, model=m0, focus="feed", test="fdr")
#' 
#' ## Two factors.
#' m1 <- lm(breaks~wool*tension, data=warpbreaks)
#' anova(m1)
#' 
#' X <- LSmatrix(m1, effect=c("wool", "tension"))
#' attributes(X)
#' 
#' Xs <- by(X, INDICES=attr(X, "grid")$tension, FUN=as.matrix)
#' Xs <- lapply(Xs, "rownames<-", levels(warpbreaks$wool))
#' 
#' lapply(Xs, apmc, model=m1, focus="wool", test="single-step",
#'        level=0.10)
#' }
apmc <- function(X, model, focus, test="single-step",
                 level=0.05){
    if(is.null(rownames(X)))
        stop("The X matrix must have row names.")
    Xc <- apc(X)
    g <- glht(model, linfct=X)
    ci <- as.data.frame(
        confint(g, level=1-level,
                calpha=univariate_calpha())$confint)
    names(ci) <- tolower(names(ci))    
    h <- summary(glht(model, linfct=Xc),
                 test=adjusted(type=test))
    h$type <- "Tukey"; h$focus <- focus
    ci$cld <- cld(h, level=level, decreasing=TRUE)$mcletters$Letters
    ci <- cbind(rownames(ci), ci)
    names(ci)[1] <- focus
    rownames(ci) <- NULL
    return(ci)
}
