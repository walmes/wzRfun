#' @title Interactive panel to estimate non linear regression models
#' 
#' @name rp.nls
#'
#' @description This function opens a interface, builted with
#' \code{rpanel} elements, designed for the estimation of non linear
#' regression models. Sliders allows the user control values used as
#' start values in the \code{nls} call, checkbox allows select strata of
#' the data and more options are available.
#'
#' @param model a non linear model formula including variables and
#' parameters to passed to \code{nls} function.
#' 
#' @param data a data frame with dependent and independent variables
#' present in the model formula.
#'
#' @param start a named list with one item for each parameter. Itens
#' names must be equal the parameters. Each item must be named vector
#' with values for the \code{init}, \code{from} and \code{to}. This is
#' the order assumed if vector are no named. These values will be used
#' by \code{rp.slider} as initial, minimum and maximum.
#' 
#' @param subset an optional string indicating a grouping variable. It
#' must be a factor.
#' 
#' @param assignTo the name of the object that will storage the value
#' returned by the \code{nls} call. It will be a list with one element
#' per level of the factor used as \code{subset}.
#'
#' @param startCurve a list with graphical definitions passed to the
#' \code{lines} function to draw the curve at the start values. The most
#' used arguments are \code{lty}, \code{col} and \code{lwd}.
#'
#' @param fittedCurve the same as \code{startCurve} but for the
#' estimated curve.
#' @param extraplot a function of the model parameters that draw
#' auxiliary graphical elements as vertical and horizontal lines as
#' references for model intercept and/or model asymptote.
#' 
#' @param finalplot the same as \code{extraplot} but uses the estimated
#' parameter values.
#'
#' @param xlab labels for the x axis.
#' 
#' @param ylab labels for the y axis.
#' 
#' @param ... arguments passed to the plot function used to draw the
#' observed values.
#'
#' @return In fact none is returned by the function. There isn't a
#' \code{return}, instead there is a \code{invisible} at the end. On the
#' other hand, the model fit is assigned to the object with name passed
#' to the \code{assingTo} argument.
#' 
#' @seealso \code{\link[stats]{nls}}, \code{\link[graphics]{lines}},
#' \code{\link[rpanel]{rp.slider}}
#'
#' @export
#' @examples
#' \donttest{
#' ##--------------------------------------------
#' ## A simple linear regression.
#' 
#' rp.nls(model=dist~b0+b1*speed,
#'        data=cars,
#'        start=list(
#'            b0=c(init=0, from=-20, to=20),
#'            b1=c(init=2, from=0, to=10)),
#'        assignTo="cars.fit")
#' 
#' cars.fit
#' 
#' ##--------------------------------------------
#' ## A more interesting example.
#' 
#' require(lattice)
#' xyplot(rate~conc, groups=state, data=Puromycin)
#' 
#' rp.nls(model=rate~Int+(Top-Int)*conc/(Half+conc),
#'        data=Puromycin,
#'        start=list(
#'            Int=c(init=50, from=20, to=70),
#'            Top=c(init=150, from=100, to=200),
#'            Half=c(init=0.1, from=0, to=0.6)),
#'        subset="state",
#'        assignTo="Puro.fit",
#'        startCurve=list(col=3, lty=3, lwd=1),
#'        fittedCurve=list(col=4, lty=1, lwd=1.5),
#'        extraplot=function(Int, Top, Half){
#'            abline(h=c(Int, Top), v=Half, col=2, lty=2)
#'        },
#'        finalplot=function(Int, Top, Half){
#'            abline(h=c(Int, Top), v=Half, col=3, lty=1)
#'        },
#'        xlab="Concentration",
#'        ylab="Rate",
#'        xlim=c(0, 1.2),
#'        ylim=c(40, 220))
#' 
#' length(Puro.fit)
#' sapply(Puro.fit, coef)
#' sapply(Puro.fit, logLik)
#' sapply(Puro.fit, deviance)
#' 
#' }
rp.nls <- function(model, data, start,
                   subset=NULL,
                   assignTo="rp.fit",
                   startCurve=list(col=2, lty=2),
                   fittedCurve=list(col=2, lty=1, lwd=1.5),
                   extraplot=NULL,
                   finalplot=NULL,
                   xlab=NULL, ylab=NULL, ...){
    
    ##----------------------------------------------------------------------
    ## Test the presence of rpanel package.
    if (!requireNamespace("rpanel", quietly=TRUE)) {
        stop("rpanel needed for this function to work. Please install it.",
             call.=FALSE)
    }

    ##----------------------------------------------------------------------
    ## Function protections. -----------------------------------------------
    ## 
    ## Dependent variable name (y).
    vardep <- all.vars(model[[2]])
    ## Independent variable name.
    varindep <- intersect(all.vars(model[[3]]), colnames(data))
    if(length(varindep)!=1)
        stop("Just one independent variable is expected.")
    ## Parameter names.
    parnames <- intersect(all.vars(model[[3]]), names(start))
    ## Test the presence of the subset variable and if it is a factor.
    if(!is.null(subset)){
        if(length(intersect(subset, names(data)))==0)
            stop("Subset variable is not present in data.")
        if(!is.factor(data[,subset]))
            stop("Subset variable must be a factor.")
    }
    ## Test if start elements are vectors of length 3.
    if(!all(sapply(start, length)==3L)){
        stop("Each element in `start` must be a vector of length 3.")
    }
    ## Test if there is names in start.
    startnames <- sapply(start,
                         function(x){
                             !(!is.null(names(x)) &
                                   all(names(x)%in%c("init","from","to")))
                         })
    if(any(startnames)){
        message(paste(collapse=" ",
            c("At least one element in `start` is not named.",
              "Using the current order to name it as init, from and to.")))
        for(j in which(startnames)){
            names(start[[j]]) <- c("init","from","to")
        }
    }
    
    ##----------------------------------------------------------------------
    ## Creating auxiliary objects. -----------------------------------------
    ## 
    ## If susbset non null, creates a list for each level, if not, a
    ## single element list.
    if(!is.null(subset)){
        aju <- vector(mode="list", length=nlevels(data[,subset]))
        names(aju) <- levels(data[,subset])
    } else {
        aju <- vector(mode="list", length=1)
    }
    ## Assign to the Global Environment.
    assign(".rpnls", aju, envir=.GlobalEnv)
    ## assign(assignTo, aju, envir=.GlobalEnv)

    ##----------------------------------------------------------------------
    ## Internal functions. -------------------------------------------------
    ## 
    ## Built a function from a model formula.
    form2func <- function(formu){
        arg1 <- all.vars(formu)
        arg2 <- vector("list", length(arg1))
        names(arg2) <- arg1
        Args <- do.call("alist", arg2)
        fmodele <- as.function(c(Args, formu))
        return(fmodele)
    }
    ## Function to get fit values from the model.
    fmodele <- form2func(model[[3]])
    ## Function that plot observed values and superpose the curve of the
    ## model.
    nlr.draw <- function(panel){
        if(is.null(xlab)){
            xlab <- varindep
        }
        if(is.null(ylab)){
            ylab <- vardep
        }
        if(is.null(panel$subset))
            plot(panel$vindep, panel$vdep,
                 xlab=xlab, ylab=ylab, ...)
        else {
            da <- data.frame(vindep=panel$vindep,
                             vdep=panel$vdep, subset=panel$subset)
            plot(vdep~vindep, data=subset(da, subset==panel$sbst),
                 xlab=xlab, ylab=ylab, ...)
        }
        vindepseq <- seq(min(panel$vindep), max(panel$vindep), length.out=101)
        listparvar <- c(list(vindepseq), panel[parnames])
        names(listparvar)[1] <- varindep
        fx <- do.call("fmodele", listparvar)
        ## lines(vindepseq, fx, col=2, lty=2)
        do.call(lines, c(list(x=vindepseq, y=fx), startCurve))
        if(panel$extraplot){
            if(!is.null(extraplot)){
                do.call(extraplot, panel[parnames])
            }}
        panel
    }
    ## Function that is called when click on "Adjust" button.
    nlsajust <- function(panel){
        ## Start values.
        nlsstart <- panel[parnames]
        ## Try to estimate paramaters with nls().
        if(is.null(panel$subset)){
            da <- data.frame(vindep=panel$vindep, vdep=panel$vdep)
            names(da) <- c(varindep, vardep)
            n0 <- try(nls(panel$model, data=da, start=nlsstart))
        } else {
            da <- data.frame(vindep=panel$vindep, vdep=panel$vdep,
                             subset=panel$subset)
            names(da) <- c(varindep, vardep, "subset")
            n0 <- try(nls(panel$model, start=nlsstart,
                          data=subset(da, subset==panel$sbst)))
        }
        ## If not converged, print error message, else superpose the
        ## estimated curve.
        if(class(n0)=="try-error"){
            par(usr=c(0, 1, 0, 1))
            text(x=0.5, y=0.5, col="red", cex=2,
                 labels="Convergence not met!\nGet closer!")
        } else {
            vindepseq <- seq(min(panel$vindep), max(panel$vindep),
                             length.out=101)
            cn0 <- as.list(coef(n0))
            listparvar <- c(list(vindepseq), cn0)
            names(listparvar)[1] <- varindep
            fx <- do.call("fmodele", listparvar)
            ## lines(vindepseq, fx, col=2)
            do.call(lines, c(list(x=vindepseq, y=fx), fittedCurve))
            if(panel$finalplot){
                if(!is.null(finalplot)){
                    do.call(finalplot, cn0)
                } else if(!is.null(extraplot)){
                    do.call(extraplot, cn0)
                }
            }
            if(is.null(panel$sbst)){
                .rpnls <<- n0
            }
            if(!is.null(panel$sbst)){
                .rpnls[[panel$sbst]] <<- n0
            }
            assign(assignTo, .rpnls, envir=.GlobalEnv)
        }
        panel
    }
    
    ##----------------------------------------------------------------------
    ## Building the controls. ----------------------------------------------
    ## 
    action <- nlr.draw
    ## Open empty window, if there is subset, creates the list box.
    if(is.null(subset)){
        nlr.panel <- rp.control(title="Adjust",
                                size=c(300, 200),
                                model=model,
                                vdep=data[,vardep],
                                vindep=data[,varindep],
                                subset=NULL)
    }
    if(!is.null(subset)){
        nlr.panel <- rp.control(title="Adjust",
                                size=c(300, 200),
                                model=model,
                                vdep=data[,vardep],
                                vindep=data[,varindep],
                                subset=data[,subset])
        rp.listbox(nlr.panel,
                   variable=sbst,
                   vals=levels(data[,subset]),
                   rows=min(c(10, nlevels(data[,subset]))),
                   title="subset",
                   action=action)
    }
    ## Create teh sliders for each parameter.
    rp.checkbox(panel=nlr.panel, variable=extraplot, action=nlr.draw,
                title="Use extraplot")
    rp.checkbox(panel=nlr.panel, variable=finalplot, action=nlsajust,
                title="Use finalplot?")
    for(i in parnames){
        callstr <- 'rp.slider(panel=nlr.panel, variable="PAR",
                              from=start[["PAR"]]["from"],
                              to=start[["PAR"]]["to"],
                              initval=start[["PAR"]]["init"],
                              showvalue=TRUE, action=action,
                              title="PAR")'
        callstr <- gsub("PAR", i, callstr)
        source(textConnection(callstr), local=TRUE)
    }
    rp.do(panel=nlr.panel, action=action)
    ## Create the `Adjust` button.
    rp.button(panel=nlr.panel, action=nlsajust, title="Adjust")
    invisible()
}
