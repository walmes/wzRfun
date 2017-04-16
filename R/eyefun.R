#' @name eyefun
#' @export
#' @author Walmes Zeviani, \email{walmes@@ufpr.br}, based on suggestion
#'     in the R-br mailing list (Brazil),
#'     \href{http://r-br.2285057.n4.nabble.com/R-br-transferir-modelo-da-funcao-nls-para-a-funcao-curve-automaticamente-td4665756.html}{\emph{[R-br]
#'     transferir modelo da função nls para a função curve()
#'     automaticamente}}.
#' @title GUI to Minipulate Non Linear Regression Models With Sliders
#' @description This function creates a GUI to manipulate parameters of
#'     a model with sliders. It is useful to get insight of a model.
#' @param model a formula with, at least, the right hand side.
#' @param start a list in many elements as number of parameters in
#'     \code{model}. Each list element must have a parameter name. All
#'     elements must be numeric vector of length 2 or 3 (\code{from},
#'     \code{init} and \code{to}) used to create the sliders with
#'     \pkg{rpanel}.
#' @param dots a named list with arguments passed to the
#'     \code{\link[graphics]{curve}()}.
#' @return A GUI with one slider for each parameter in \code{model} to
#'     manipulate the curve of the model.
#' @importFrom rpanel rp.slider rp.control
#' @examples
#'
#' \donttest{
#'
#' library(rpanel)
#'
#' model <- y ~ A/(1 + exp(-(x - M)/S))
#' start <- list(A = c(2, 5),
#'               M = c(0, 5),
#'               S = c(-1, 0.5, 1))
#' eyefun(model, start,
#'        dots = list(lwd = 2, col = "blue",
#'                    from = 0, to = 10,
#'                    ylim = c(0, 5),
#'                    xlab = "Age", ylab = "Weight",
#'                    main = expression(alpha/(1 + exp(-(x - M)/S)))))
#'
#' }
eyefun <- function(model, start, dots = NULL) {
    # Test if package is available.
    if (!requireNamespace("rpanel", quietly = TRUE)) {
        stop(paste0("`rpanel` needed for this function to work.",
                    " Please install it."),
             call. = FALSE)
    }
    # Test and stardization of start sets.
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
    # Test if model is a formula.
    if(!class(model) == "formula") {
        stop("`model` must be a formula.")
    }
    # Number of terms in the model.
    k <- length(model)
    # Right hand side of formula.
    vars <- all.vars(model[[k]])
    # Parameter names.
    params <- vars %in% names(start)
    parnames <- vars[params]
    # Independent variable name.
    varindep <- vars[!params]
    if (length(varindep)!=1){
        stop("just one independent variable is expected!")
    }
    # Dependent variable name.
    if (k == 3) {
        vardep <- all.vars(model[[2]])
    } else {
        vardep <-  "y"
    }
    if (is.null(dots$ylab)) {
        dots$ylab <- vardep
    }
    #----------------------------------------
    # Reactive function.
    nlr.draw <- function(panel) {
        with(panel,
        {
            do.call("curve",
                    args = c(expr = model[[3]],
                             xname = varindep,
                             dots))
        })
        return(panel)
    }
    #----------------------------------------
    # Set GUI.
    nlr.panel <- rp.control(title = "Ajuste",
                            size = c(200, 200),
                            model = model,
                            varindep = varindep,
                            dots = dots)
    # Add sliders.
    for (i in parnames) {
        callstr <- 'rp.slider(panel = nlr.panel,
                              variable = "PAR",
                              from = start[["PAR"]]["from"],
                              to = start[["PAR"]]["to"],
                              initval = start[["PAR"]]["init"],
                              showvalue = TRUE,
                              action = nlr.draw,
                              title = "PAR")'
        callstr <- gsub("PAR", i, callstr)
        source(textConnection(callstr), local = TRUE)
    }
    rp.do(panel = nlr.panel, action = nlr.draw)
    invisible()
}
