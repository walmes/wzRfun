#' @author Walmes Zeviani, \email{walmes@@ufpr.br}.
#' @export
#' @title Generate Matrix of All Pairwise Comparisons (Tukey contrasts)
#' @description This function takes a matrix where each line defines a
#'     linear function of the parameters to estimate a marginal mean
#'     (aka least squares mean) and return the matrix that define the
#'     contrasts among these means. All pairwise contrasts are returned
#'     (aka Tukey contrasts). The matrix with these contrasts can be
#'     passed to \code{\link[multcomp]{glht}()} to estimate them or used
#'     in explicit matricial calculus.
#' @param lfm a \eqn{k \times p} matrix where each line defines a linear
#'     function to estimate a lsmean. In general, these matrices are
#'     obtained by using \code{\link[doBy]{LE_matrix}()}.
#' @param lev a character vector with length equals to the numbers of
#'     lines of \code{lfm} matrix, (\eqn{k}). Default is \code{NULL} and
#'     the row names of code{lfm} is used. If row names is also
#'     \code{NULL}, incremental integer values are used to identify the
#'     comparisons.
#' @return a \eqn{K\times p} matrix with the linear functions that
#'     define all pairwise contrasts. \eqn{K} is \eqn{{k}\choose{2}}.
#' @seealso \code{\link{apmc}()}, \code{\link[doBy]{LE_matrix}()}.
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
#' # Objects from doBy::LE_matrix() have an "grid" attribute.
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
    cbn <- utils::combn(seq_along(lev), 2)
    M <- lfm[cbn[1, ], ] - lfm[cbn[2, ], ]
    if (is.vector(M)) {
        dim(M) <- c(1, length(M))
    }
    rownames(M) <- paste(lev[cbn[1, ]], lev[cbn[2, ]], sep = "-")
    return(M)
}

#' @author Walmes Zeviani, \email{walmes@@ufpr.br}.
#' @export
#' @title A Wraper of glht to Get All Pairwise Mean Comparisons
#' @description This function performs all pairwise compararisons among
#'     means returning pontual and intervalar estimates followed by
#'     letters to easy discriminate values. It is in fact a wraper of
#'     \code{\link[multcomp]{glht}()}.
#' @param X a matrix where each line is a linear function of the model
#'     parameters to estimate a least squares mean. In most pratical
#'     cases, it is an object from the \code{\link[doBy]{LE_matrix}()}.
#' @param model a model with class recognized by
#'     \code{\link[multcomp]{glht}()}.
#' @param focus a string with the name of the factor which levels are
#'     being compared.
#' @param test a p-value correction method. See
#'     \code{\link[stats]{p.adjust.methods}()}.
#' @param level the experimentwise significance level for the multiple
#'     comparisons. The individual coverage of the confidence interval
#'     is \code{1-level}. Default is 0.05.
#' @param cld2 Logical, if \code{TRUE} uses the \code{\link{cld2}()}
#'     functions, otherwise uses the \code{\link[multcomp]{cld}()}
#'     function.
#' @return a \code{data.frame} with interval estimates and compact
#'     letter display for the means comparisons.
#' @seealso \code{\link{apc}()}, \code{\link[doBy]{LE_matrix}()},
#'     \code{\link[multcomp]{glht}()}.
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
#' L <- LE_matrix(m0, effect = "feed")
#' rownames(L) <- levels(chickwts$feed)
#' apmc(L, model = m0, focus = "feed", test = "fdr")
#'
#' data(warpbreaks)
#'
#' # Two factors (complete factorial).
#' m1 <- lm(breaks ~ wool * tension, data = warpbreaks)
#' anova(m1)
#'
#' L <- LE_matrix(m1, effect = c("wool", "tension"))
#' attributes(L)
#'
#' Ls <- by(L, INDICES = attr(L, "grid")$tension, FUN = as.matrix)
#' Ls <- lapply(Ls, "rownames<-", levels(warpbreaks$wool))
#'
#' # Comparing means of wool in each tension.
#' lapply(Ls, apmc, model = m1, focus = "wool",
#'        test = "single-step", level = 0.1)
#'
#' # Two factors (incomplete factorial).
#' warpbreaks <- subset(warpbreaks, !(tension == "H" & wool == "A"))
#' xtabs(~tension + wool, data = warpbreaks)
#'
#' # There is NA in the estimated parameters.
#' m2 <- lm(breaks ~ wool * tension, data = warpbreaks)
#' coef(m2)
#'
#' X <- model.matrix(m2)
#' b <- coef(m2)
#'
#' X <- X[, !is.na(b)]
#'
#' # unique(X)
#'
#' # Uses the full estimable model matriz.
#' m3 <- update(m2, . ~ 0 + X)
#'
#' # These models are in fact the same.
#' anova(m2, m3)
#'
#' # LS matrix has all cells.
#' L <- LE_matrix(m2, effect = c("wool", "tension"))
#' g <- attr(L, "grid")
#' L <- L[, !is.na(b)]
#' i <- 5
#' L <- L[-i, ]
#' g <- g[-i, ]
#'
#' rownames(L) <- make.names(g$tension, unique = FALSE)
#' Ls <- split.data.frame(L, g$wool)
#'
#' # LSmeans with MCP test.
#' lapply(Ls, apmc, model = m3, focus = "tension",
#'        test = "single-step", level = 0.1, cld2 = TRUE)
#'
#' # Sample means.
#' aggregate(breaks ~ tension + wool, data = warpbreaks, FUN = mean)
#'
apmc <- function(X,
                 model,
                 focus,
                 test = "single-step",
                 level = 0.05,
                 cld2 = FALSE) {
    if (is.null(rownames(X))) {
        stop("The X matrix must have row names.")
    }
    Xc <- apc(X)
    g <- multcomp::glht(model, linfct = X)
    ci <- stats::confint(g,
                         level = 1 - level,
                         calpha = multcomp::univariate_calpha())$confint
    ci <- as.data.frame(ci)
    names(ci) <- tolower(names(ci))
    names(ci)[1] <- "fit"
    h <- summary(multcomp::glht(model, linfct = Xc),
                 test = multcomp::adjusted(type = test))
    h$type <- "Tukey"
    h$focus <- focus
    if (cld2) {
        ci$cld <- cld2(h, level = level)$mcletters$Letters
    } else {
        ci$cld <- multcomp::cld(h, level = level,
                                decreasing = TRUE)$mcletters$Letters
    }
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
#'     factorial designs and nested factorial designs. These models are
#'     usually declared by a model matrix to have all effects
#'     estimated. It is assumed that Tukey contrasts was used.
#' @param object an object returned by \code{\link[multcomp]{glht}()}. It
#'     is assumed that the matrix used as the \code{linfct} argument in
#'     \code{glht} corresponds to a matrix to get Tukey contrasts of
#'     least squares means.
#' @param level the nominal significance level.
#' @return an object of class \code{"cld"} with letters to resume mean
#'     comparisons.
#' @seealso \code{\link{apc}()}, \code{\link[doBy]{LE_matrix}()},
#'     \code{\link[multcomp]{glht}()}.
# @import multcomp
#' @examples
#'
#' # Toy data 1: experiment with cultivars in several locations.
#' td1 <- expand.grid(loc = gl(5, 1),
#'                    block = gl(3, 1),
#'                    cult = LETTERS[1:6])
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
#' X <- LE_matrix(lm(nobars(formula(m1)), data = td1), effect = "loccult")
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
cld2 <- function(object,
                 level = 0.05) {
    lvl_order <- unique(unlist(
        strsplit(rownames(object$linfct), "-")))
    signif <- (object$test$pvalues < level)
    ret <- list()
    ret$signif <- signif
    ret$comps <- do.call(rbind,
                         strsplit(rownames(object$linfct), "-"))
    ret$mcletters <-
        insert_absorb(x = signif,
                      decreasing = TRUE,
                      comps = ret$comps,
                      lvl_order = lvl_order)
    class(ret) <- "cld"
    return(ret)
}

#' @name ordered_cld
#' @author Walmes Zeviani, \email{walmes@@ufpr.r}.
#' @export
#' @title Order Letters According to Numeric Vector
#' @description This function order the letters in the compact letter
#'     display to the highest estimate receive the letter \code{a}. This
#'     is a convetion in most software for analysis of experiments.
#' @param let Character vector with the letters returned by
#'     \code{\link[multcomp]{cld}()} or \code{\link{cld2}()}.
#' @param means Numeric vector with the corresponding estimates in which
#'     the highest value will have the letter \code{a}.
#' @return A character vector with the letters rearranged.
#' @seealso \code{\link{cld2}()}.
#' @examples
#'
#' # Toy data.
#' set.seed(4321)
#' td <- data.frame(trt = rep(sample(1:8), each = 5))
#' td$y <- rnorm(nrow(td), mean = sort(td$trt), sd = 2)
#'
#' plot(y ~ trt, data = td)
#'
#' # Fit the model.
#' td$trt <- factor(td$trt)
#' m0 <- lm(y ~ trt, data = td)
#' anova(m0)
#' summary(m0)
#'
#' library(multcomp)
#' library(doBy)
#'
#' X <- LE_matrix(m0, effect = "trt")
#' rownames(X) <- levels(td$trt)
#' Xc <- apc(X)
#'
#' g <- summary(glht(m0, linfct = Xc),
#'              test = adjusted(type = "fdr"))
#'
#' res <- data.frame(trt = levels(td$trt),
#'                   mean = X %*% coef(m0))
#'
#' let <- cld2(g)
#' res$cld2 <- let$mcletters$Letters
#' res[order(res$mean, decreasing = TRUE), ]
#'
#' res$let2 <- ordered_cld(res$cld2, res$mean)
#' res[order(res$mean, decreasing = TRUE), ]
#'
#' \dontrun{
#'
#' library(latticeExtra)
#' library(grid)
#'
#' ci <- as.data.frame(
#'     confint(glht(m0, linfct = X),
#'             calpha = univariate_calpha())$confint)
#' ci <- cbind(res, ci)
#'
#' segplot(reorder(trt, Estimate) ~ lwr + upr,
#'         centers = Estimate,
#'         data = ci,
#'         draw = FALSE,
#'         cld = ci$let2,
#'         par.settings = list(layout.widths = list(right.padding = 7))) +
#'     layer(panel.text(x = centers,
#'                      y = z,
#'                      labels = sprintf("%0.2f %s",
#'                                       centers,
#'                                       cld),
#'                      pos = 3))
#'
#' ocld <- with(ci[order(ci$Estimate), ],
#'      ordered_cld(cld2, Estimate))
#' x <- attr(ocld, "ind")
#' index <- which(x, arr.ind = TRUE)
#' trellis.focus("panel", column = 1, row = 1, clip.off = TRUE)
#' xcor <- 1.03 + (index[, 2] - 1)/50
#' grid.segments(x0 = unit(xcor, "npc"),
#'               x1 = unit(xcor, "npc"),
#'               y0 = unit(index[, 1] + 0.5, units = "native"),
#'               y1 = unit(index[, 1] - 0.5, units = "native"),
#'               gp = gpar(lwd = 2, col = "blue"))
#' trellis.unfocus()
#'
#' }
#'
ordered_cld <- function(let, means = let) {
    or <- order(means, decreasing = TRUE)
    let <- as.character(let[or])
    s <- strsplit(let, "")
    u <- unlist(s)
    ul <- unique(u)
    UL <- LETTERS[seq_along(ul)]
    l <- sapply(s, FUN = function(i) {
        paste(sort(UL[match(i, table = ul)]), collapse = "")
    })
    x <- tolower(l[order(or)])
    UL <- tolower(UL)
    attr(x, "match") <- cbind("before" = ul,
                              "after" = UL)
    attr(x, "ind") <- sapply(UL, FUN = grepl, x = x)
    return(x)
}

#' @name radial_cld
#' @importFrom graphics lines plot points segments text
#' @author Walmes Zeviani, \email{walmes@@ufpr.r}.
#' @export
#' @title Radial Plot for a Compact Letter Display Vector
#' @description This function does a radial plot based on the vector of
#'     letters resulted from pairwise comparisons.
#' @param cld Character vector with strings of letters that indicates
#'     which pair of treatment cells are not different.
#' @param labels Vector of text to be annotated next each point.
#' @param col Vector of colors to be used in the segments that joint
#'     points.
#' @param means Numeric vector with the estimated means of treatment
#'     cells. It is used to place points at distances proportional to
#'     the differences on means.
#' @param perim Logical value (default is \code{FALSE}) that indicates
#'     weather draw or not a circle in the perimeter passing by the
#'     points.
#' @param legend Logical value (default is \code{TRUE}) that indicates
#'     weather daraw or not the legend.
#' @return None is returned, only the plot is done.
#' @seealso \code{\link{cld2}()}.
#' @examples
#'
#' set.seed(4321)
#' td <- data.frame(trt = rep(sample(1:20), each = 5))
#' td$y <- rnorm(nrow(td), mean = 0.15 * sort(td$trt), sd = 1)
#'
#' plot(y ~ trt, data = td)
#'
#' # Fit the model.
#' td$trt <- factor(td$trt)
#' m0 <- lm(y ~ trt, data = td)
#' anova(m0)
#' summary(m0)
#'
#' library(multcomp)
#' library(doBy)
#'
#' X <- LE_matrix(m0, effect = "trt")
#' rownames(X) <- levels(td$trt)
#'
#' ci <- apmc(X, m0, focus = "trt", test = "fdr")
#' ci$cld <- with(ci, ordered_cld(cld, fit))
#' ci <- ci[order(ci$fit, decreasing = TRUE), ]
#'
#' library(latticeExtra)
#'
#' segplot(reorder(trt, fit) ~ lwr + upr,
#'         centers = fit,
#'         data = ci,
#'         draw = FALSE,
#'         cld = ci$cld) +
#'     layer(panel.text(x = centers,
#'                      y = z,
#'                      labels = sprintf("%0.2f %s",
#'                                       centers,
#'                                       cld),
#'                      pos = 3))
#'
#' radial_cld(cld = ci$cld)
#' radial_cld(cld = ci$cld, means = ci$fit, perim = TRUE)
#' radial_cld(cld = ci$cld, col = 1:3)
#' radial_cld(cld = ci$cld, col = 1:3)
#' radial_cld(cld = ci$cld, labels = sprintf("%0.2f %s", ci$fit, ci$cld))
#'
radial_cld <- function(cld,
                       labels = cld,
                       col = NULL,
                       means = NULL,
                       perim = FALSE,
                       legend = TRUE) {
    if (is.null(means)) {
        s <- seq(from = 0,
                 to = 2 * pi,
                 length.out = length(cld) + 1)[-1]
    } else {
        ext <- (2 * pi)/c(length(cld), 1)
        m <- means - min(means)
        m <- m/max(m)
        s <- ext[1] + diff(ext) * m
        s <- rev(s)
    }
    sincos <- cbind(sin = sin(s), cos = cos(s))
    # Quais as letras únicas formadoras das strings?
    u <- unique(unlist(strsplit(cld, split = "")))
    if (is.null(col)) {
        col <- grDevices::palette()
    }
    if (length(col) != length(u)) {
        warning(paste("Length of vector `col` is different",
                      "of the number of unique letters.",
                      "Colors will be recycled."))
    }
    col <- col[seq_along(u) %% length(col) + 1]
    # Membros da mesma família compartilham a mesma letra.
    fam <- sapply(u, grepl, x = cld)
    plot(x = NULL,
         y = NULL,
         xlim = 1.2 * c(-1, 1),
         ylim = 1.2 * c(-1, 1),
         asp = 1,
         axes = FALSE,
         ann = FALSE)
    if (perim) {
        circ <- seq(0, 2 * pi, length.out = 60)
        lines(x = sin(circ), y = cos(circ), col = "gray", lty = 3)
    }
    for (i in 1:ncol(fam)) {
        cb <- utils::combn(x = which(fam[, i]), m = 2)
        apply(cb,
              MARGIN = 2,
              FUN = function(index) {
                  segments(x0 = sincos[index[1], 1],
                           x1 = sincos[index[2], 1],
                           y0 = sincos[index[1], 2],
                           y1 = sincos[index[2], 2],
                           col = col[i],
                           lwd = 2,
                           lty = 2)
              })
    }
    points(x = sincos[, 1], y = sincos[, 2])
    if (legend) {
        legend("topright",
               legend = u,
               col = col,
               lty = 2,
               lwd = 2,
               bty = "n")
    }
    text(sincos[, 1],
         sincos[, 2],
         labels = labels,
         pos = ifelse(sincos[, 1] > 0, 4, 2))
}
