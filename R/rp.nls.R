#' @name rp.nls
#' @export
#' @author Walmes Zeviani, \email{walmes@ufpr.br}.
#' @title Interactive Fitting for Non Linear Regression Models
#' @description This function opens a interface, builted with
#'     \pkg{rpanel} elements, designed for non linear regression models
#'     fitting. Sliders allows the user control values used as start
#'     values in the \code{\link[stats]{nls}} call, checkbox allows
#'     select strata of the data to fit separed curves and more options
#'     are available.
#' @param model a non linear model formula including variables and
#'     parameters to passed to \code{\link[stats]{nls}} function.
#' @param data a data frame with dependent and independent variables
#'     present in the model formula.
#' @param start a named list with one item for each parameter. Each item
#'     must vector with 2 (from, to) or 3 values (from, to, init) for
#'     the \code{init}, \code{from} and \code{to}. When two values are
#'     provided, the third is the average of them. Values are sorted
#'     internally to match the initial, minimum and maximum to be used
#'     by \code{\link[rpanel]{rp.slider}}.
#' @param subset an optional string indicating a grouping variable to
#'     fit separeted curves for each level. It must be a factor.
#' @param assign the name of the object that will storage the value
#'     returned by the \code{\link[stats]{nls}} call. It will be a list
#'     with one element per level of the factor used as \code{subset}.
#' @param start_curve a list with graphical definitions passed to the
#' @param xlab labels for the x axis.
#' @param ylab labels for the y axis.
#' @param ... arguments passed to the plot function used to draw the
#'     \code{lines} function to draw the curve at the start values. The
#'     most used arguments are \code{lty}, \code{col} and \code{lwd}.
#' @param fitted_curve the same as \code{start_curve} but for the fitted
#'     curve.
#' @param extra_plot a function of the model parameters that draw
#'     auxiliary graphical elements. For example, draw vertical and
#'     horizontal lines as references for model intercept and/or model
#'     asymptote.
#' @param final_plot the same as \code{extra_plot} but uses the
#'     estimated parameter values.  observed values.
#' @return In fact none is returned by the function. There isn't a
#'     \code{return}, instead there is a \code{invisible} at the end. On
#'     the other hand, the model fit is assigned to the object with name
#'     passed to the \code{assign} argument.
#' @seealso \code{\link[stats]{nls}}, \code{\link[graphics]{lines}},
#'     \code{\link[rpanel]{rp.slider}}
#' @import rpanel graphics stats
#' @examples
#'
#' \donttest{
#'
#' #--------------------------------------------
#' # A simple linear regression.
#'
#' # The model below is a linear model. You should fit in lm().
#'
#' library(rpanel)
#'
#' rp.nls(model = dist ~ b0 + b1 * speed, data = cars,
#'        start = list(b0 = c(-20, 20),
#'                     b1 = c(0, 2, 10)),
#'        assign = "cars.fit",
#'        xlab = "Speed", ylab = "Distance")
#'
#' summary(cars.fit)
#' confint(cars.fit)
#'
#' f <- formula(cars.fit)
#'
#' plot(dist ~ speed, data = cars)
#' with(as.list(coef(cars.fit)), {
#'     eval(call("curve", f[[3]], add = TRUE,
#'               xname= intersect(all.vars(f[-2]), names(cars))))
#' })
#'
#' #--------------------------------------------
#' # A non linear regression.
#'
#' data(turk0, package = "alr3")
#' str(turk0)
#'
#' plot(Gain ~ A, data = turk0,
#'      xlab = "Metionine", ylab = "Weight gain")
#'
#' rp.nls(model = Gain ~ Int + (Asy - Int) * A/(Half + A), data = turk0,
#'        start = list(Int = c(600, 650),
#'                     Asy = c(750, 850),
#'                     Half = c(0, 0.2)),
#'        assign = "turk.fit",
#'        xlab = "Metionine", ylab = "Weight gain")
#'
#' summary(turk.fit)
#' confint(turk.fit)
#'
#' f <- formula(turk.fit)
#'
#' plot(Gain ~ A, data = turk0,
#'      xlab = "Metionine", ylab = "Weight gain")
#' with(as.list(coef(turk.fit)), {
#'     eval(call("curve", f[[3]], add = TRUE,
#'               xname= intersect(all.vars(f[-2]), names(turk0))))
#' })
#'
#' #--------------------------------------------
#' # A more interesting example.
#'
#' library(lattice)
#'
#' xyplot(rate ~ conc, groups = state, data = Puromycin,
#'        type = c("p", "smooth"), auto.key = TRUE)
#'
#' rp.nls(model = rate ~ Int + (Top - Int) * conc/(Half + conc),
#'        data = Puromycin,
#'        start = list(Int = c(20, 70),
#'                     Top = c(100, 200),
#'                     Half = c(0, 0.6)),
#'        subset = "state",
#'        assign = "Puro.fit",
#'        start_curve = list(col = 3, lty = 3, lwd = 1),
#'        fitted_curve = list(col = 4, lty = 1, lwd = 1.5),
#'        extra_plot = function(Int, Top, Half) {
#'            abline(h = c(Int, Top), v = Half, col = 2, lty = 2)
#'        },
#'        final_plot = function(Int, Top, Half) {
#'            abline(h = c(Int, Top), v = Half, col = 3, lty = 1)
#'        },
#'        xlab = "Concentration", ylab = "Rate",
#'        xlim = c(0, 1.2), ylim = c(40, 220))
#'
#' length(Puro.fit)
#' sapply(Puro.fit, coef)
#' sapply(Puro.fit, logLik)
#' sapply(Puro.fit, deviance)
#'
#' }
rp.nls <- function(model, data, start,
                   subset = NULL,
                   assign = "rp.fit",
                   xlab = NULL, ylab = NULL,  ...,
                   start_curve = list(col = 2, lty = 2),
                   fitted_curve = list(col = 2, lty = 1, lwd = 1.5),
                   extra_plot = NULL,
                   final_plot = NULL){

    #-------------------------------------------------------------------
    # Test the presence of rpanel package.
    if (!requireNamespace("rpanel", quietly = TRUE)) {
        stop(paste0("rpanel needed for this function to work.",
                    " Please install it."),
             call. = FALSE)
    }

    #-------------------------------------------------------------------
    # Function protections.

    # Test on start values.
    start <- lapply(start,
                    FUN = function(x) {
                        stopifnot(is.numeric(x))
                        stopifnot(any(length(x) == c(2, 3)))
                        if (length(x) == 2) {
                            x <- c(x, mean(x))
                        }
                        x <- sort(x)
                        names(x) <- c("from", "init", "to")
                        return(x)
                    })

    # Dependent variable name (y).
    vardep <- all.vars(model[[2]])

    # Independent variable name.
    varindep <- intersect(all.vars(model[[3]]), colnames(data))
    if (length(varindep) != 1) {
        stop("Just one independent variable is expected.")
    }

    # Parameter names.
    parnames <- intersect(all.vars(model[[3]]), names(start))

    # Test the presence of the subset variable and if it is a factor.
    if (!is.null(subset)) {
        if (length(intersect(subset, names(data))) == 0) {
            stop("Subset variable is not present in data.")
        }
        if (!is.factor(data[, subset])) {
            stop("Subset variable must be a factor.")
        }
    }

    #-------------------------------------------------------------------
    # Creating auxiliary objects.

    # If susbset non null, creates a list for each level, if not, a
    # single element list.
    if (is.null(subset)) {
        aju <- vector(mode = "list", length = 1)
    } else {
        aju <- vector(mode = "list",
                      length = nlevels(data[, subset]))
        names(aju) <- levels(data[, subset])
    }

    # Assign to the Global Environment.
    assign(".rpnls", aju, envir = .GlobalEnv)

    #-------------------------------------------------------------------
    # Internal functions.

    # Built a function from a model formula (based on the nlstools
    # package).
    form2func <- function(formu) {
        arg1 <- all.vars(formu)
        arg2 <- vector("list", length(arg1))
        names(arg2) <- arg1
        Args <- do.call("alist", arg2)
        fmodele <- as.function(c(Args, formu))
        return(fmodele)
    }

    # Function to get fit values from the model.
    fmodele <- form2func(model[[3]])

    # Function that plot observed values and superpose the curve of the
    # model.
    nlr.draw <- function(panel) {
        if (is.null(xlab)) {
            xlab <- varindep
        }
        if (is.null(ylab)) {
            ylab <- vardep
        }
        if (is.null(panel$subset)) {
            plot(panel$vindep, panel$vdep,
                 xlab = xlab, ylab = ylab, ...)
        } else {
            da <- data.frame(vindep = panel$vindep, vdep = panel$vdep,
                             subset = panel$subset)
            plot(vdep ~ vindep,
                 data = subset(da, subset == panel$sbst),
                 xlab = xlab, ylab = ylab, ...)
        }
        vindepseq <- seq(min(panel$vindep), max(panel$vindep),
                         length.out = 101)
        listparvar <- c(list(vindepseq), panel[parnames])
        names(listparvar)[1] <- varindep
        fx <- do.call("fmodele", listparvar)
        do.call(lines, c(list(x = vindepseq, y = fx), start_curve))
        if (!is.null(extra_plot)) {
            if (panel$extra_plot) {
                do.call(extra_plot, panel[parnames])
            }
        }
        panel
    }

    # Function that is called when click on "Adjust" button.
    nlsajust <- function(panel) {
        # Start values.
        nlsstart <- panel[parnames]
        # Try to estimate paramaters with nls().
        if (is.null(panel$subset)) {
            da <- data.frame(vindep = panel$vindep, vdep = panel$vdep)
            names(da) <- c(varindep, vardep)
            n0 <- try(nls(panel$model, data = da, start = nlsstart))
        } else {
            da <- data.frame(vindep = panel$vindep, vdep = panel$vdep,
                             subset = panel$subset)
            names(da) <- c(varindep, vardep, "subset")
            n0 <- try(nls(panel$model, start = nlsstart,
                          data = subset(da, subset == panel$sbst)))
        }
        # If not converged, print error message, else superpose the
        # estimated curve.
        if (class(n0) == "try-error") {
            par(usr = c(0, 1, 0, 1))
            text(x = 0.5, y = 0.5, col = "red", cex = 2,
                 labels = "Convergence not met!\nGet closer!")
        } else {
            vindepseq <- seq(min(panel$vindep), max(panel$vindep),
                             length.out = 101)
            cn0 <- as.list(coef(n0))
            listparvar <- c(list(vindepseq), cn0)
            names(listparvar)[1] <- varindep
            fx <- do.call("fmodele", listparvar)
            do.call(lines, c(list(x = vindepseq, y = fx), fitted_curve))
            if (!is.null(final_plot) && panel$final_plot) {
                do.call(final_plot, cn0)
            }
            if (is.null(panel$subset)) {
                .rpnls <<- n0
            }
            if (!is.null(panel$subset)) {
                .rpnls[[panel$sbst]] <<- n0
            }
            assign(assign, .rpnls, envir = .GlobalEnv)
        }
        return(panel)
    }

    #----------------------------------------
    # Building the controls.

    action <- nlr.draw

    # Open empty window, if there is subset, creates the list box.
    if (is.null(subset)) {
        nlr.panel <- rp.control(title = "rp.nls",
                                size = c(300, 200),
                                model = model,
                                vdep = data[, vardep],
                                vindep = data[, varindep],
                                subset = NULL)
    }
    if (!is.null(subset)) {
        nlr.panel <- rp.control(title = "rp.nls",
                                size = c(300, 200),
                                model = model,
                                vdep = data[, vardep],
                                vindep = data[, varindep],
                                subset = data[, subset])
        rp.listbox(nlr.panel, variable = sbst,
                   vals = levels(data[, subset]),
                   rows = min(c(10, nlevels(data[, subset]))),
                   title = "subset",
                   action = action)
    }

    # Create the sliders for each parameter.
    for (i in parnames) {
        callstr <- 'rp.slider(panel = nlr.panel,
                              variable = "PAR",
                              from = start[["PAR"]]["from"],
                              to = start[["PAR"]]["to"],
                              initval = start[["PAR"]]["init"],
                              showvalue = TRUE,
                              action = action,
                              title = "PAR")'
        callstr <- gsub("PAR", i, callstr)
        source(textConnection(callstr), local = TRUE)
    }
    if (!is.null(extra_plot)) {
        rp.checkbox(panel = nlr.panel,
                    variable = extra_plot,
                    action = nlr.draw,
                    title = "Use extra_plot")
    }
    if (!is.null(final_plot)) {
        rp.checkbox(panel = nlr.panel,
                    variable = final_plot,
                    action = nlsajust,
                    title = "Use final_plot?")
    }
    rp.do(panel = nlr.panel, action = action)

    # Create the `Adjust` button.
    rp.button(panel = nlr.panel, action = nlsajust, title = "Adjust")
    invisible()
}
