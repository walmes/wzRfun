#' @name rp.nls
#' @export
#' @author Walmes Zeviani, \email{walmes@ufpr.br}.
#' @title Interactive Fitting for Non Linear Regression Models
#' @description This function opens a interface, builted with
#'     \pkg{rpanel} elements, designed for non linear regression models
#'     fitting. Sliders allows the user control values used as start
#'     values in the \code{\link[stats]{nls}()} call, checkbox allows
#'     select strata of the data to fit separed curves and more options
#'     are available.
#'
#' \if{html}{\figure{rp-nls.png}{options: width="671px"}}
#' \if{latex}{\figure{rp-nls.pdf}{options: width=5.4in}}
#' @param model a non linear model formula including variables and
#'     parameters to passed to \code{\link[stats]{nls}()} function.
#' @param data a data frame with dependent and independent variables
#'     present in the model formula.
#' @param start a named list with one item for each parameter. Each item
#'     must vector with 2 (from, to) or 3 values (from, to, init) for
#'     the \code{init}, \code{from} and \code{to}. When two values are
#'     provided, the third is the average of them. Values are sorted
#'     internally to match the initial, minimum and maximum to be used
#'     by \code{\link[rpanel]{rp.slider}()}.
#' @param subset an optional string indicating a grouping variable to
#'     fit separeted curves for each level. It must be a factor.
#' @param xlab labels for the x axis.
#' @param ylab labels for the y axis.
#' @param ... arguments passed to the plot function used to draw the
#'     \code{lines} function to draw the curve at the start values. The
#'     most used arguments are \code{lty}, \code{col} and \code{lwd}.
#' @param gpar a named list with graphical parameters for curve. The
#'     \code{"try"} element is a list for the eye curve that is
#'     manipulated with sliders. The \code{"fit"} is a list for the
#'     fitted curve.
#' @param extra a function of the model parameters that draw auxiliary
#'     graphical elements. For example, draw vertical and horizontal
#'     lines as references for model intercept and/or model asymptote.
#' @return In fact none is returned by the function. There isn't a
#'     \code{return}, instead there is a \code{invisible} at the end. On
#'     the other hand, the model fit is assigned to the object with name
#'     passed to the \code{assign} argument.
#' @seealso \code{\link[stats]{nls}()}, \code{\link[graphics]{lines}()},
#'     \code{\link[rpanel]{rp.slider}()}
#' @keywords GUI
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
#' cars.fit <- rp.nls(model = dist ~ b0 + b1 * speed, data = cars,
#'                    start = list(b0 = c(-20, 20),
#'                                 b1 = c(0, 2, 10)),
#'                    xlab = "Speed", ylab = "Distance")
#'
#' summary(cars.fit)
#' confint(cars.fit)
#'
#' f <- formula(cars.fit)
#'
#' plot(dist ~ speed, data = cars)
#' with(as.list(coef(cars.fit)), {
#'     eval(call("curve", f[[3]], add = TRUE,
#'               xname = intersect(all.vars(f[-2]), names(cars))))
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
#' turk.fit <- rp.nls(model = Gain ~ Int + (Asy - Int) * A/(Half + A),
#'                    data = turk0,
#'                    start = list(Int = c(600, 650),
#'                                 Asy = c(750, 850),
#'                                 Half = c(0, 0.2)),
#'                    extra = function(Int, Asy, Half) {
#'                        abline(h = c(Asy, Int), v = Half,
#'                               col = "green")
#'                    },
#'                    xlab = "Metionine", ylab = "Weight gain")
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
#'               xname = intersect(all.vars(f[-2]), names(turk0))))
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
#' Puro.fit <- rp.nls(model = rate ~
#'                        Int + (Top - Int) * conc/(Half + conc),
#'                    data = Puromycin,
#'                    start = list(Int = c(20, 70),
#'                                 Top = c(100, 200),
#'                                 Half = c(0, 0.6)),
#'                    subset = "state",
#'                    gpar = list(try = list(col = 2, lty = 2),
#'                                fit = list(col = "blue", lwd = 1.5)),
#' #                  extra = function(Int, Top, Half) {
#' #                      abline(h = c(Int, Top), v = Half,
#' #                             col = 2, lty = 2)
#' #                  },
#'                    xlab = "Concentration", ylab = "Rate",
#'                    xlim = c(0, 1.2), ylim = c(40, 220))
#'
#' length(Puro.fit)
#' sapply(Puro.fit, coef)
#' sapply(Puro.fit, logLik)
#' sapply(Puro.fit, deviance)
#'
#' }
rp.nls <- function(model, data, start,
                   subset = NULL,
                   xlab = NULL, ylab = NULL,  ...,
                   gpar = list(try = list(col = 2, lty = 2),
                               fit = list(col = 2, lwd = 1.5)),
                   extra = NULL){

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
        FIT <- vector(mode = "list", length = 1)
    } else {
        FIT <- vector(mode = "list",
                      length = nlevels(data[, subset]))
        names(FIT) <- levels(data[, subset])
    }

    #-------------------------------------------------------------------
    # Internal functions.

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
            graphics::plot(panel$vdep ~ panel$vindep,
                           xlab = xlab, ylab = ylab, ...)
        } else {
            da <- data.frame(vindep = panel$vindep,
                             vdep = panel$vdep,
                             subset = panel$subset)
            graphics::plot(vdep ~ vindep,
                           data = subset(da, subset == panel$sbst),
                           xlab = xlab, ylab = ylab, ...)
        }

        with(panel, {
            do.call("curve",
                    args = c(expr = model[[3]],
                             xname = varindep,
                             add = TRUE, gpar$try))
        })

        if (!is.null(extra)) {
            do.call(what = "extra", panel[parnames])
        }
        return(panel)
    }

    # Function that is called when click on "Adjust" button.
    nlsajust <- function(panel) {
        # Start values.
        nlsstart <- panel[parnames]
        # Try to estimate paramaters with nls().
        if (is.null(panel$subset)) {
            da <- data.frame(vindep = panel$vindep, vdep = panel$vdep)
            names(da) <- c(varindep, vardep)
            n0 <- try(stats::nls(panel$model,
                                 data = da,
                                 start = nlsstart))
        } else {
            da <- data.frame(vindep = panel$vindep, vdep = panel$vdep,
                             subset = panel$subset)
            names(da) <- c(varindep, vardep, "subset")
            n0 <- try(stats::nls(panel$model,
                                 start = nlsstart,
                                 data = subset(da, subset == panel$sbst)))
        }
        # If not converged, print error message, else superpose the
        # estimated curve.
        if (class(n0) == "try-error") {
            graphics::par(usr = c(0, 1, 0, 1))
            graphics::text(x = 0.5, y = 0.5, col = "red", cex = 2,
                           labels = "Convergence not met!\nGet closer!")
        } else {
            cn0 <- as.list(stats::coef(n0))
            with(cn0, {
                do.call("curve",
                        args = c(expr = model[[3]],
                                 xname = varindep,
                                 add = TRUE, gpar$fit))
            })
            if (!is.null(extra)) {
                do.call(what = "extra", cn0)
            }
            # Assign values to FIT in the parent environment.
            if (is.null(panel$subset)) {
                FIT <<- n0
            }
            if (!is.null(panel$subset)) {
                FIT[[panel$sbst]] <<- n0
            }
        }
        return(panel)
    }

    #----------------------------------------
    # Building the controls.

    nlr.panel <- rpanel::rp.control(title = "rp.nls",
                                    size = c(300, 200),
                                    model = model,
                                    vdep = data[, vardep],
                                    vindep = data[, varindep],
                                    subset = if (is.null(subset)) {
                                                 NULL
                                             } else {
                                                 data[, subset]
                                             })
    rpanel::rp.text(panel = nlr.panel,
                    text = paste0("Don't quit closing the window.\n",
                                  "Click on `Save and Quit` button."))
    rpanel::rp.button(panel = nlr.panel,
                      title = "Save and Quit",
                      background = "yellow",
                      action = function(panel) {
                          assign("finish", value = FALSE,
                                 envir = parent.env(environment()))
                          rpanel::rp.control.dispose(nlr.panel)
                      })
    if (!is.null(subset)) {
        rpanel::rp.listbox(nlr.panel,
                           variable = "sbst",
                           vals = levels(data[, subset]),
                           rows = min(c(10, nlevels(data[, subset]))),
                           title = "subset",
                           action = nlr.draw)
    }
    # Create the sliders for each parameter.
    for (i in parnames) {
        callstr <- 'rpanel::rp.slider(panel = nlr.panel,
                                      variable = "PAR",
                                      from = start[["PAR"]]["from"],
                                      to = start[["PAR"]]["to"],
                                      initval = start[["PAR"]]["init"],
                                      showvalue = TRUE,
                                      action = nlr.draw,
                                      title = "PAR")'
        callstr <- gsub("PAR", i, callstr)
        source(textConnection(callstr), local = TRUE)
        closeAllConnections()
    }
    rpanel::rp.button(panel = nlr.panel,
                      action = nlsajust,
                      title = "Adjust")
    rpanel::rp.do(panel = nlr.panel,
                  action = nlr.draw)
    finish <- TRUE
    while (finish) { }
    if (exists("FIT")) {
        return(FIT)
    } else {
        NULL
    }
}
