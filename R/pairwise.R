#' @name apc
#' @author Walmes Zeviani, \email{walmes@@ufpr.br}.
#' @export
#' @title Generate Matrix of All Pairwise Comparisons (Tukey contrasts)
#' @description This function takes a matrix where each line defines a
#'     linear function of the parameters to estimate a marginal mean
#'     (aka least squares mean) and return the matrix that define the
#'     contrasts among these means. All pairwise contrasts are returned
#'     (aka Tukey contrasts). The matrix with these contrasts can be
#'     passed to \code{\link[multcomp]{glht}} to estimate them or used
#'     in explicit matricial calculus.
#' @param lfm a \eqn{k \times p} matrix where each line defines a linear
#'     function to estimate a lsmean. In general, these matrices are
#'     obtained by using \code{\link[doBy]{LSmatrix}}.
#' @param lev a character vector with length equals to the numbers of
#'     lines of \code{lfm} matrix, (\eqn{k}). Default is \code{NULL} and
#'     the row names of code{lfm} is used. If row names is also
#'     \code{NULL}, incremental integer values are used to identify the
#'     comparisons.
#' @return a \eqn{K\times p} matrix with the linear functions that
#'     define all pairwise contrasts. \eqn{K} is \eqn{{k}\choose{2}}.
#' @seealso \code{\link{apmc}}, \code{\link[doBy]{LSmatrix}}.
#' @examples
#'
#' X <- diag(3)
#' rownames(X)
#' apc(X)
#'
#' rownames(X) <- letters[nrow(X):1]
#' apc(X)
#'
#' apc(X, lev = LETTERS[1:nrow(X)])
#'
#' # Objects from doBy::LSmatrix() have an "grid" attribute.
#' attr(X, "grid") <- data.frame(n = LETTERS[1:nrow(X)])
#' rownames(X) <- NULL
#' apc(X)
#'
apc <- function(lfm, lev = NULL) {
    nlev <- nrow(lfm)
    rn <- rownames(lfm)
    a <- attr(lfm, "grid")
    if (is.null(lev)) {
        if (!is.null(a)) {
            lev <- apply(a, 1, paste, collapse = ":")
        } else if (!is.null(rn)) {
            lev <- rn
        } else {
            lev <- as.character(1:nlev)
        }
    }
    cbn <- combn(seq_along(lev), 2)
    M <- lfm[cbn[1, ], ] - lfm[cbn[2, ], ]
    if (is.vector(M)) {
        dim(M) <- c(1, length(M))
    }
    rownames(M) <- paste(lev[cbn[1, ]], lev[cbn[2, ]], sep = "-")
    return(M)
}

#' @name apmc
#' @author Walmes Zeviani, \email{walmes@@ufpr.br}.
#' @export
#' @title A Wraper of glht to Get All Pairwise Mean Comparisons
#' @description This function performs all pairwise compararisons among
#'     means returning pontual and intervalar estimates followed by
#'     letters to easy discriminate values. It is in fact a wraper of
#'     \code{\link[multcomp]{glht}}.
#' @param X a matrix where each line is a linear function of the model
#'     parameters to estimate a least squares mean. In most pratical
#'     cases, it is an object from the \code{\link[doBy]{LSmatrix}}.
#' @param model a model with class recognized by
#'     \code{\link[multcomp]{glht}}.
#' @param focus a string with the name of the factor which levels are
#'     being compared.
#' @param test a p-value correction method. See
#'     \code{\link[stats]{p.adjust.methods}}.
#' @param level the experimentwise significance level for the multiple
#'     comparisons. The individual coverage of the confidence interval
#'     is \code{1-level}. Default is 0.05.
#' @return a \code{data.frame} with interval estimates and compact
#'     letter display for the means comparisons.
#' @seealso \code{\link{apc}}, \code{\link[doBy]{LSmatrix}},
#'     \code{\link[multcomp]{glht}}.
#' @examples
#'
#' library(doBy)
#' library(multcomp)
#'
#' # Single factor.
#' m0 <- lm(weight ~ feed, data = chickwts)
#' anova(m0)
#'
#' # Prepare the matrix to estimate lsmeans.
#' X <- LSmatrix(m0, effect = "feed")
#' rownames(X) <- levels(chickwts$feed)
#' apmc(X, model = m0, focus = "feed", test = "fdr")
#'
#' # Two factors.
#' m1 <- lm(breaks ~ wool * tension, data = warpbreaks)
#' anova(m1)
#'
#' X <- LSmatrix(m1, effect = c("wool", "tension"))
#' attributes(X)
#'
#' Xs <- by(X, INDICES = attr(X, "grid")$tension, FUN = as.matrix)
#' Xs <- lapply(Xs, "rownames<-", levels(warpbreaks$wool))
#'
#' # Comparing means of wool in each tension.
#' lapply(Xs, apmc, model = m1, focus = "wool",
#'        test = "single-step", level = 0.1)
#'
apmc <- function(X, model, focus, test = "single-step", level = 0.05) {
    if (is.null(rownames(X))) {
        stop("The X matrix must have row names.")
    }
    Xc <- apc(X)
    g <- multcomp::glht(model, linfct = X)
    ci <- confint(g, level = 1 - level,
                  calpha = multcomp::univariate_calpha())$confint
    ci <- as.data.frame(ci)
    names(ci) <- tolower(names(ci))
    names(ci)[1] <- "fit"
    h <- summary(multcomp::glht(model, linfct = Xc),
                 test = adjusted(type = test))
    h$type <- "Tukey"
    h$focus <- focus
    ci$cld <- multcomp::cld(h, level = level,
                            decreasing = TRUE)$mcletters$Letters
    ci <- cbind(rownames(ci), ci)
    names(ci)[1] <- focus
    rownames(ci) <- NULL
    return(ci)
}

#' @name cld2
#' @author Walmes Zeviani, \email{walmes@@ufpr.r}.
#' @export
#' @title Modified Compact Letter Display to Irregular Designs
#' @description This functions get the compact letter display for
#'     objects of class \code{"glht"}. Modification was done to get the
#'     letters to design with missing cells, non completelly crossed
#'     factorial designs and nested factorial designs. It is assumed
#'     that Tukey contrasts was used.
#' @param object an object returned by \code{\link[multcomp]{glht}}. It
#'     is assumed that the matrix used as the \code{linfct} argument in
#'     \code{glht} corresponds to a matrix to get Tukey contrasts of
#'     least squares means.
#' @param level the nominal significance level.
#' @return an object of class \code{"cld"} with letters to resume mean
#'     comparisons.
#' @seealso \code{\link{apc}}, \code{\link[doBy]{LSmatrix}},
#'     \code{\link[multcomp]{glht}}.
#' @import multcomp
#' @examples
#'
#' # Toy data 1: experiment with cultivars in several locations.
#' td1 <- expand.grid(loc = gl(5, 1), block = gl(3, 1), cult = LETTERS[1:6])
#' td1 <- subset(td1, !(loc == 1 & cult == "A"))
#' td1 <- subset(td1, !(loc == 2 & cult == "B"))
#' xtabs(~loc + cult, td1)
#' td1$y <- seq_len(nrow(td1))
#'
#' library(lme4)
#'
#' # Fit the mixed model.
#' m0 <- lmer(y ~ loc * cult + (1 | loc:block), data = td1)
#' logLik(m0)
#'
#' # The same model but without rank deficience.
#' td1$loccult <- with(td1, interaction(loc, cult, drop = TRUE))
#' m1 <- lmer(y ~ loccult + (1 | loc:block), data = td1)
#' logLik(m1)
#'
#' library(doBy)
#'
#' X <- LSmatrix(lm(nobars(formula(m1)), data = td1), effect = "loccult")
#' rownames(X) <- levels(td1$loccult)
#' dim(X)
#'
#' Xs <- X[grepl(x = rownames(X), "^1\\."),]
#' Xc <- apc(Xs)
#'
#' library(multcomp)
#'
#' g <- summary(glht(m1, linfct = Xc), test = adjusted(type = "fdr"))
#'
#' cld2(g)
#'
#' confint(glht(m1, linfct = Xs), calpha = univariate_calpha())
#'
cld2 <- function(object, level = 0.05) {
    lvl_order <- unique(unlist(
        strsplit(rownames(object$linfct), "-")))
    signif <- (object$test$pvalues < level)
    ret <- list()
    ret$signif <- signif
    ret$comps <- do.call(rbind,
                         strsplit(rownames(object$linfct), "-"))
    ret$mcletters <-
        multcomp:::insert_absorb(x = signif,
                                 decreasing = TRUE,
                                 comps = ret$comps,
                                 lvl_order = lvl_order)
    class(ret) <- "cld"
    return(ret)
}
