#' @title Modified compact letter dysplay
#' 
#' @name cld2
#'
#' @description This functions get the compact letter display for
#' objects of class \code{glht}. Modification was done to get the
#' letters to design with missing cells, non completelly crossed
#' factorial designs and nested factorial designs. It is assumed that
#' Tukey contrasts was used.
#'
#' @param object an object returned by \code{glht}. It is assumed that
#' the matrix used as the \code{linfct} argument in glht correspond a
#' matriz to get Tukey contrasts between least squares means.
#'
#' @param level the nominal significance level.
#'
#' @return an object of class \code{cld} with letters to resume mean
#' comparisons.
#' 
#' @seealso \code{\link{apc}}, \code{\link[doBy]{LSmatrix}},
#' \code{\link[multcomp]{glht}}.
#'
#' @import multcomp
#' 
#' @export
#' @examples
#' ## Toy data 1: experiment with cultivars in several locations.
#' td1 <- expand.grid(loc=gl(5,1), block=gl(3,1), cult=LETTERS[1:6])
#' td1 <- subset(td1, !(loc==1 & cult=="A"))
#' td1 <- subset(td1, !(loc==2 & cult=="B"))
#' xtabs(~loc+cult, td1)
#' td1$y <- seq_len(nrow(td1))
#' 
#' require(lme4)
#' 
#' ## Fit the mixed model.
#' m0 <- lmer(y~loc*cult+(1|loc:block), data=td1)
#' logLik(m0)
#' 
#' ## The same model but without rank deficience.
#' td1$loccult <- with(td1, interaction(loc, cult, drop=TRUE))
#' m1 <- lmer(y~loccult+(1|loc:block), data=td1)
#' logLik(m1)
#' 
#' require(doBy)
#' 
#' X <- LSmatrix(lm(nobars(formula(m1)), data=td1), effect="loccult")
#' rownames(X) <- levels(td1$loccult)
#' dim(X)
#' 
#' Xs <- X[grepl(x=rownames(X), "^1\\."),]
#' 
#' require(wzRfun)
#' 
#' Xc <- apc(Xs)
#' 
#' require(multcomp)
#' 
#' g <- summary(glht(m1, linfct=Xc), test=adjusted(type="fdr"))
#' 
#' cld2(g)
#' 
#' confint(glht(m1, linfct=Xs), calpha=univariate_calpha())
cld2 <- function(object, level=0.05){
    lvl_order <- unique(unlist(strsplit(rownames(object$linfct), "-")))
    signif <- (object$test$pvalues < level)
    ret <- list()
    ret$signif <- signif
    ret$comps <- do.call(rbind, strsplit(rownames(object$linfct), "-"))
    ret$mcletters <- multcomp:::insert_absorb(
        x=signif, decreasing=TRUE,
        comps=ret$comps, lvl_order=lvl_order)
    class(ret) <- "cld"
    ret
}
