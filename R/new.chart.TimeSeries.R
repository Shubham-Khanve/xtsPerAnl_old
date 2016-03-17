#' A New chart.Timeseries Function
#'
#' This function allows you to plot timeseries and is based on xts::plot.xts rather than base graphics.
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' @param auto.grid if true, draws a grid aligned with the points on the x and
#' y axes
#' @param grid.color sets the color for the reference grid
#' @param grid.lty defines the line type for the grid
#' @param yaxis.right if true, draws the y axis on the right-hand side of the
#' plot
#' @param type set the chart type, same as in \code{\link{plot}}
#' @param lty set the line type, same as in \code{\link{plot}}
#' @param lwd set the line width, same as in \code{\link{plot}}
#' @param las set the axis label rotation, same as in \code{\link{plot}}
#' @param main set the chart title, same as in \code{\link{plot}}
#' @param ylab set the y-axis label, same as in \code{\link{plot}}
#' @param date.format re-format the dates for the xaxis
#' @param xlim set the x-axis limit, same as in \code{\link{plot}}
#' @param ylim set the y-axis limit, same as in \code{\link{plot}}
#' @param colorset color palette to use, set by default to rational choices
#' @param pch symbols to use, see also \code{\link{plot}}
#' @param element.color provides the color for drawing chart elements, such as
#' the box lines, axis lines, etc. Default is "darkgray"
#' @param legend.loc places a legend into one of nine locations on the chart:
#' bottomright, bottom, bottomleft, left, topleft, top, topright, right, or
#' center.
#' @param ylog TRUE/FALSE set the y-axis to logarithmic scale, similar to
#' \code{\link{plot}}, default FALSE
#' @param date.format.in allows specification of other date formats in the data
#' object, defaults to "\%Y-\%m-\%d"
#' @param cex.axis The magnification to be used for axis annotation relative to
#' the current setting of 'cex', same as in \code{\link{plot}}.
#' @param major.ticks Should major tickmarks be drawn and labeled, default
#' 'auto'
#' @param xaxis.labels Allows for non-date labeling of date axes, default is
#' NULL
#' @param \dots any other passthru parameters
#' @keywords new.chart.TimeSeries
#' @export
#' @examples
#' library(xtsPerAnl)
#' library(xts)  #library(xtsExtra)
#' library(PerformanceAnalytics)
#' data(managers)
#' new.chart.TimeSeries(managers)
new.chart.TimeSeries <-
function (R, 
          auto.grid=TRUE, 
          xaxis = TRUE, 
          yaxis = TRUE, 
          yaxis.right = FALSE, 
          type = "l", 
          lty = 1, 
          lwd = 2, 
          las = par("las"),
          main = NULL, 
          ylab=NULL, 
          xlab="", 
          date.format.in="%Y-%m-%d", 
          date.format = NULL, 
          xlim = NULL, 
          ylim = NULL, 
          element.color="darkgray", 
          event.lines = NULL, 
          event.labels = NULL, 
          period.areas = NULL, 
          event.color = "darkgray", 
          period.color = "aliceblue", colorset = (1:12), 
          pch = (1:12), 
          legend.loc = NULL, 
          ylog = FALSE, 
          cex.axis=0.8, 
          cex.legend = 0.8, 
          cex.lab = 1, 
          cex.labels = 0.8, 
          cex.main = 1, 
          major.ticks='auto', 
          minor.ticks=TRUE, 
          grid.color="lightgray", 
          grid.lty="dotted", 
          xaxis.labels = NULL, ...){
		  
		  y = checkData(R)
		  
#################################################################################
new.axTicksByTime2 <- function (x, ticks.on = "auto", k = 1, labels = TRUE, 
                            format.labels = TRUE,  ends = TRUE, 
                            gt = 2, lt = 25){
    if (timeBased(x)) 
        x <- xts(rep(1, length(x)), x)
    tick.opts <- c("years", "months", "weeks", "days")
    tick.k.opts <- c(1,1,1,1)
    if (ticks.on %in% tick.opts) {
        cl <- ticks.on[1]
        ck <- k
    }
    else {
        tick.opts <- paste(tick.opts, tick.k.opts)
        is <- structure(rep(0, length(tick.opts)), .Names = tick.opts)
        for (i in 1:length(tick.opts)) {
            y <- strsplit(tick.opts[i], " ")[[1]]
            ep <- endpoints(x, y[1], as.numeric(y[2]))
            if(i>1 && is[i-1] == length(ep)-1)
                break
            is[i] <- length(ep) - 1
            if (is[i] > lt)
                break
        }
        nms <- rev(names(is)[which(is > gt & is < lt)])[1]
        cl <- strsplit(nms, " ")[[1]][1]
        ck <- as.numeric(strsplit(nms, " ")[[1]][2])
    }
    if (is.na(cl) || is.na(ck) || is.null(cl)) {
        return(c(1,NROW(x)))
    }
    else ep <- endpoints(x, cl, ck)
    if (ends) 
        ep <- ep + c(rep(1, length(ep) - 1), 0)
    if (labels) {
        if (is.logical(format.labels) || is.character(format.labels)) {
            unix <- ifelse(.Platform$OS.type == "unix", TRUE, 
                           FALSE)
            fmt <- switch(cl,
                          "years"="%Y",
                          "months"="%b",
                          "days"="%d",
                          "weeks"="W%W",
                          "hours"="%H:%M",
                          "minutes"="%H:%M:%S",
                          "seconds"="%H:%M:%S")
            if(ndays(x) > 1 && cl %in% c("hours","minutes","seconds")) {
                fmt <- paste("%b-%d",fmt)
            }
            names(ep) <- format(index(x)[ep], fmt)
        }
        else names(ep) <- as.character(index(x)[ep])
    }
    ep
}

###################################################################################

.plotxtsEnv <- new.env()
current.xts_chob <- function() invisible(get(".xts_chob",.plotxtsEnv))

###################################################################################

new.chart.lines <- function(x, 
                        type="l", 
                        lty=1,
                        lwd=2,
                        lend=1,
                        col=1:10, 
                        up.col=NULL, 
                        dn.col=NULL,
                        legend.loc=NULL,
                        pch=1){
    if(is.null(up.col)) up.col <- "green"
    if(is.null(dn.col)) dn.col <- "red"
    xx <- current.xts_chob()
    if(type == "h"){
        colors <- ifelse(x[,1] < 0, dn.col, up.col)
        lines(xx$Env$xycoords$x,x[,1],lwd=2,col=colors,lend=lend,lty=1,type="h")
    } else if(type == "l" || type == "p") {
        if(length(lty) == 1) lty <- rep(lty, NCOL(x))
        if(length(lwd) == 1) lwd <- rep(lwd, NCOL(x))
        for(i in NCOL(x):1){
            lines(xx$Env$xycoords$x, x[,i], type=type, lend=lend, col=col[i], lty=lty[i], lwd=lwd[i], pch=pch)
        }
    } else if(type == "bar"){
        positives = negatives = x
        for(column in 1:NCOL(x)){
            for(row in 1:NROW(x)){ 
                positives[row,column] = max(0, x[row,column])
                negatives[row,column] = min(0, x[row,column])
            }
        }
        barplot.default(t(positives), add=TRUE, col=col, axisnames=FALSE, axes=FALSE)
        barplot.default(t(negatives), add=TRUE, col=col, axisnames=FALSE, axes=FALSE)
    }
    if(!is.null(legend.loc)){
        yrange <- range(x, na.rm=TRUE)
        chob.xlim <- xx$Env$xlim
        switch(legend.loc,
               topleft = {
                   xjust <- 0
                   yjust <- 1
                   lx <- chob.xlim[1]
                   ly <- yrange[2]
               },
               left = {
                   xjust <- 0
                   yjust <- 0.5
                   lx <- chob.xlim[1]
                   ly <- sum(yrange) / 2
               },
               bottomleft = {
                   xjust <- 0
                   yjust <- 0
                   lx <- chob.xlim[1]
                   ly <- yrange[1]
               },
               top = {
                   xjust <- 0.5
                   yjust <- 1
                   lx <- (chob.xlim[1] + chob.xlim[2]) / 2
                   ly <- yrange[2]
               },
               center = {
                   xjust <- 0.5
                   yjust <- 0.5
                   lx <- (chob.xlim[1] + chob.xlim[2]) / 2
                   ly <- sum(yrange) / 2
               },
               bottom = {
                   xjust <- 0.5
                   yjust <- 0
                   lx <- (chob.xlim[1] + chob.xlim[2]) / 2
                   ly <- yrange[1]
               },
               topright = {
                   xjust <- 1
                   yjust <- 1
                   lx <- chob.xlim[2]
                   ly <- yrange[2]
               },
               right = {
                   xjust <- 1
                   yjust <- 0.5
                   lx <- chob.xlim[2]
                   ly <- sum(yrange) / 2
               },
               bottomright = {
                   xjust <- 1
                   yjust <- 0
                   lx <- chob.xlim[2]
                   ly <- yrange[1]
               }
        )
        legend(x=lx, y=ly, legend=colnames(x), xjust=xjust, yjust=yjust, 
               fill=col[1:NCOL(x)], bty="n")
    }
}

#################################################################################

new.replot_xts <- function(frame=1,asp=1,xlim=c(1,10),ylim=list(structure(c(1,10),fixed=FALSE))) {
    Env <- new.env()
    Env$frame <- frame
    Env$asp   <- asp
    Env$xlim  <- xlim
    Env$ylim  <- ylim
    Env$pad1 <- -0 
    Env$pad3 <-  0  
    if(length(asp) != length(ylim))
        stop("'ylim' and 'asp' must be the same length")
  
    set_frame <- function(frame,clip=TRUE) { 
        Env$frame <<- frame; 
        set_window(clip); 
    }
    set_asp   <- function(asp) { Env$asp <<- asp }
    set_xlim  <- function(xlim) { Env$xlim <<- xlim }
    set_ylim  <- function(ylim) { Env$ylim <<- ylim }
    set_pad   <- function(pad) { Env$pad1 <<- pad[1]; Env$pad3 <<- pad[2] }
    reset_ylim <- function() {
        ylim <- get_ylim()
        ylim <- rep(list(c(Inf,-Inf)),length(ylim))
        lapply(Env$actions,
               function(x) {
                   frame <- attr(x, "frame")
                   if(frame > 0) {
                       lenv <- attr(x,"env")
                       if(is.list(lenv)) lenv <- lenv[[1]]
                       ylim[[frame]][1] <<- min(ylim[[frame]][1],range(na.omit(lenv$xdata[Env$xsubset]))[1],na.rm=TRUE)
                       ylim[[frame]][2] <<- max(ylim[[frame]][2],range(na.omit(lenv$xdata[Env$xsubset]))[2],na.rm=TRUE)
                   }
               })
        set_ylim(ylim)
    }
    
    get_frame <- function(frame) { Env$frame }
    get_asp   <- function(asp) { Env$asp }
    get_xlim  <- function(xlim) { Env$xlim }
    get_ylim  <- function(ylim) { Env$ylim }
    get_pad   <- function() c(Env$pad1,Env$pad3)
    
    scale_ranges <- function(frame, asp, ranges)
    {
        asp/asp[frame] * abs(diff(ranges[[frame]]))
    }
    set_window <- function(clip=TRUE,set=TRUE)
    {
        frame <- Env$frame
        frame <- abs(frame)
        asp   <- Env$asp
        xlim  <- Env$xlim
        ylim  <- lapply(Env$ylim, function(x) structure(x + (diff(x) * c(Env$pad1, Env$pad3)),fixed=attr(x,"fixed")))
        sr <- scale_ranges(frame, asp, ylim)
        if(frame == 1) {
            win <- list(xlim, c((ylim[[frame]][1] - sum(sr[-1])), ylim[[frame]][2]))
        } else
            if(frame == length(ylim)) {
                win <- list(xlim, c(ylim[[frame]][1], ylim[[frame]][2] + sum(sr[-length(sr)])))
            } else {
                win <- list(xlim, c(ylim[[frame]][1] - sum(sr[-(1:frame)]),
                                    ylim[[frame]][2] + sum(sr[-(frame:length(sr))])))
            }
        if(!set) return(win)
        do.call("plot.window",win)
        if(clip) clip(par("usr")[1],par("usr")[2],ylim[[frame]][1],ylim[[frame]][2])
    }
    
    get_actions <- function(frame) {
        actions <- NULL
        for(i in 1:length(Env$actions)) {
            if(abs(attr(Env$actions[[i]],"frame"))==frame)
                actions <- c(actions, Env$actions[i])
        }
        actions
    }
    
    add_frame <- function(after, ylim=c(0,0), asp=0, fixed=FALSE) {
        if(missing(after))
            after <- max(abs(sapply(Env$actions, function(x) attr(x,"frame"))))
        for(i in 1:length(Env$actions)) {
            cframe <- attr(Env$actions[[i]],"frame")
            if(cframe > 0 && cframe > after)
                attr(Env$actions[[i]], "frame") <- cframe+1L
            if(cframe < 0 && cframe < -after)
                attr(Env$actions[[i]], "frame") <- cframe-1L
        }
        Env$ylim <- append(Env$ylim,list(structure(ylim,fixed=fixed)),after)
        Env$asp  <- append(Env$asp,asp,after)
    }
    update_frames <- function(headers=TRUE) {
        from_by <- ifelse(headers,2,1)  
        ylim <- get_ylim()
        for(y in seq(from_by,length(ylim),by=from_by)) {
            if(!attr(ylim[[y]],'fixed'))
                ylim[[y]] <- structure(c(Inf,-Inf),fixed=FALSE)
        }
        lapply(Env$actions,
               function(x) {
                   if(!is.null(attr(x,"no.update")) && attr(x, "no.update"))
                       return(NULL)
                   frame <- abs(attr(x, "frame"))
                   fixed <- attr(ylim[[frame]],'fixed')
                   #fixed <- attr(x, "fixed")
                   if(frame %% from_by == 0 && !fixed) {
                       lenv <- attr(x,"env")
                       if(is.list(lenv)) lenv <- lenv[[1]]
                       dat.range <- range(na.omit(lenv$xdata[Env$xsubset]))
                       min.tmp <- min(ylim[[frame]][1],dat.range,na.rm=TRUE)
                       max.tmp <- max(ylim[[frame]][2],dat.range,na.rm=TRUE)
                       ylim[[frame]] <<- structure(c(min.tmp,max.tmp),fixed=fixed)
                   }
               })
     
        set_ylim(ylim)
    }
    remove_frame <- function(frame) {
        rm.frames <- NULL
        max.frame <- max(abs(sapply(Env$actions, function(x) attr(x,"frame"))))
        for(i in 1:length(Env$actions)) {
            cframe <- attr(Env$actions[[i]],"frame")
            if(abs(attr(Env$actions[[i]],"frame"))==frame)
                rm.frames <- c(rm.frames, i)
            if(cframe > 0 && cframe > frame) {
                attr(Env$actions[[i]], "frame") <- cframe-1L
            }
            if(cframe < 0 && cframe < -frame) {
                attr(Env$actions[[i]], "frame") <- cframe+1L
            }
        }
        if(frame > max.frame) {
            Env$frame <- max.frame
        } else Env$frame <- max.frame-1
        Env$ylim <- Env$ylim[-frame]
        Env$asp  <- Env$asp[-frame]
        if(!is.null(rm.frames))
            Env$actions <- Env$actions[-rm.frames]
    }
    next_frame <- function() {
        set_frame(max(abs(sapply(Env$actions,function(x) attr(x,"frame"))))+1L)
    }
    move_frame   <- function() {}
   
    Env$actions <- list()
 
    add <- replot <- function(x,env=Env,expr=FALSE,clip=TRUE,...) {
        if(!expr) {
            x <- match.call()$x
        } 
        a <- structure(x,frame=Env$frame,clip=clip,env=env,...)
        Env$actions[[length(Env$actions)+1]] <<- a
    }
    
    replot_env <- new.env()
    class(replot_env) <- c("replot_xts","environment")
    replot_env$Env <- Env
    replot_env$set_window <- set_window
    replot_env$add <- add
    replot_env$replot <- replot
    replot_env$get_actions <- get_actions
    replot_env$subset <- subset
    replot_env$update_frames <- update_frames
    replot_env$set_frame <- set_frame
    replot_env$get_frame <- get_frame
    replot_env$next_frame <- next_frame
    replot_env$add_frame <- add_frame
    replot_env$remove_frame <- remove_frame
    replot_env$set_asp <- set_asp
    replot_env$get_asp <- get_asp
    replot_env$set_xlim <- set_xlim
    replot_env$get_xlim <- get_xlim
    replot_env$reset_ylim <- reset_ylim
    replot_env$set_ylim <- set_ylim
    replot_env$get_ylim <- get_ylim
    replot_env$set_pad <- set_pad
    return(replot_env)
}

###################################################################################

new.plot.xts <- function(x,  ############ Indicates parameters taken from new.Chart.TimeSeries()
                     y=NULL,
                     ...,
                     subset="",
                     FUN=NULL,
                     panels=NULL,
                     multi.panel=FALSE,
                     col=colorset,		######
                     up.col="green",
                     dn.col="red",
                     type=type,   #######
                     lty=lty,	######
                     lwd=lwd,	######
                     lend=1,
                     main=deparse(substitute(x)),  
                     clev=0,
                     cex=0.6, 
                     cex.axis=cex.axis,		######
                     mar=c(3,2,0,2), 
                     srt=0,
                     observation.based=FALSE,
                     xaxis.las=las,	######
                     ylim=ylim,	#####
                     yaxis.same=TRUE,
                     yaxis.left=TRUE,
                     yaxis.right=yaxis.right,  ######
                     grid.ticks.on="months",
                     grid.ticks.lwd=1,
                     grid.ticks.lty=1,
                     grid.col=grid.color,	#######
                     labels.col="#333333",
                     format.labels=TRUE,
                     shading=1,
                     bg.col="#FFFFFF",
                     grid2="#F5F5F5",
                     legend.loc=legend.loc){	#######
    
    if(is.numeric(multi.panel)){
        multi.panel <- min(NCOL(x), multi.panel)
        idx <- seq.int(1L, NCOL(x), 1L)
        chunks <- split(idx, ceiling(seq_along(idx)/multi.panel))
        
        if(!is.null(panels) && nchar(panels) > 0){
            multi.panel <- FALSE
        } else {
            multi.panel <- TRUE
            panels <- NULL
            
            if(yaxis.same){
                if(!is.null(FUN) && nchar(FUN) > 0){
                    fun <- match.fun(FUN)
                    .formals <- formals(fun)
                    .formals <- modify.args(formals=.formals, arglist=list(...), dots=TRUE)
                    if("R" %in% names(.formals)) .formals <- modify.args(formals=.formals, arglist=NULL, R=x, dots=TRUE)
                    .formals$... <- NULL
                    R <- try(do.call(fun, .formals), silent=TRUE)
                    if(inherits(R, "try-error")) { 
                        message(paste("FUN function failed with message", R))
                        ylim <- range(x[subset], na.rm=TRUE)
                    } else {
                        ylim <- range(R[subset], na.rm=TRUE)
                    }
                } else {
                    ylim <- range(x[subset], na.rm=TRUE)
                }
            }
        }
        
        for(i in 1:length(chunks)){
            tmp <- chunks[[i]]
            p <- new.plot.xts(x=x[,tmp], 
                          y=y,
                          ...=...,
                          subset=subset,
                          FUN=FUN,
                          panels=panels,
                          multi.panel=multi.panel,
                          col=col,
                          up.col=up.col,
                          dn.col=dn.col,
                          type=type,
                          lty=lty,
                          lwd=lwd,
                          lend=lend,
                          main=main,  
                          clev=clev,
                          cex=cex, 
                          cex.axis=cex.axis,
                          mar=mar, 
                          srt=srt,
                          observation.based=observation.based,
                          xaxis.las=xaxis.las,
                          ylim=ylim,
                          yaxis.same=yaxis.same,
                          yaxis.left=yaxis.left,
                          yaxis.right=yaxis.right,
                          grid.ticks.on=grid.ticks.on,
                          grid.ticks.lwd=grid.ticks.lwd,
                          grid.ticks.lty=grid.ticks.lty,
                          grid.col=grid.col,
                          labels.col=labels.col,
                          format.labels=format.labels,
                          shading=shading,
                          bg.col=bg.col,
                          grid2=grid2,
                          legend.loc=legend.loc)
            if(i < length(chunks))
                print(p)
        }
        return(p)
    }
    
    cs <- new.replot_xts()
    if(is.null(grid.ticks.on)) {
        xs <- x[subset]
        major.grid <- c(years=nyears(xs),
                        months=nmonths(xs),
                        days=ndays(xs))
        grid.ticks.on <- names(major.grid)[rev(which(major.grid < 30))[1]]
    } 
    cs$subset <- function(x) {
        if(FALSE) {set_ylim <- get_ylim <- set_xlim <- Env <-function(){} }  # appease R parser?
        if(missing(x)) {
            x <- "" 
        }
        Env$xsubset <<- x
        set_xlim(range(Env$xycoords$x, na.rm=TRUE))
        ylim <- get_ylim()
        for(y in seq(2,length(ylim),by=2)) {
            if(!attr(ylim[[y]],'fixed'))
                ylim[[y]] <- structure(c(Inf,-Inf),fixed=FALSE)
        }
        lapply(Env$actions,
               function(x) {
                   frame <- abs(attr(x, "frame"))
                   fixed <- attr(ylim[[frame]],'fixed')
                   if(frame %% 2 == 0 && !fixed) {
                       lenv <- attr(x,"env")
                       if(is.list(lenv)) lenv <- lenv[[1]]
                       yrange <- range(lenv$xdata[Env$xsubset], na.rm=TRUE)
                       if(all(yrange == 0)) yrange <- yrange + c(-1,1)
                       min.tmp <- min(ylim[[frame]][1],yrange[1],na.rm=TRUE)
                       max.tmp <- max(ylim[[frame]][2],yrange[2],na.rm=TRUE)
                       ylim[[frame]] <<- structure(c(min.tmp,max.tmp),fixed=fixed)
                   }
               })
        set_ylim(ylim)
    }
    environment(cs$subset) <- environment(cs$get_asp)
    
    if(multi.panel){
        cs$set_asp(NCOL(x))
    } else {
        cs$set_asp(3)
    }
    cs$Env$cex <- cex
    cs$Env$mar <- mar
    cs$Env$clev = min(clev+0.01,1) # (0,1]
    cs$Env$theme$shading <- shading
    cs$Env$theme$up.col <- up.col
    cs$Env$theme$dn.col <- dn.col
    if (hasArg(colorset)){
        cs$Env$theme$col <- match.call(expand.dots=TRUE)$colorset
    } else {
        cs$Env$theme$col <- col
    }
    cs$Env$theme$rylab <- yaxis.right
    cs$Env$theme$lylab <- yaxis.left
    cs$Env$theme$bg <- bg.col
    cs$Env$theme$grid <- grid.col
    cs$Env$theme$grid2 <- grid2
    cs$Env$theme$labels <- labels.col
    cs$Env$theme$srt <- srt
    cs$Env$theme$xaxis.las <- xaxis.las
    cs$Env$theme$cex.axis <- cex.axis
    cs$Env$format.labels <- format.labels
    cs$Env$grid.ticks.on <- grid.ticks.on
    cs$Env$grid.ticks.lwd <- grid.ticks.lwd
    cs$Env$grid.ticks.lty <- grid.ticks.lty
    cs$Env$type <- type
    cs$Env$lty <- lty
    cs$Env$lwd <- lwd
    cs$Env$lend <- lend
    cs$Env$legend.loc <- legend.loc
    cs$Env$call_list <- list()
    cs$Env$call_list[[1]] <- match.call()
    cs$Env$observation.based <- observation.based
    
    if(is.character(x))
        stop("'x' must be a time-series object")
    cs$Env$xdata <- x
    cs$Env$xsubset <- subset
    cs$Env$column_names <- colnames(x)
    cs$Env$nobs <- NROW(cs$Env$xdata)
    cs$Env$main <- main
    if(cs$Env$observation.based){
        cs$Env$xycoords <- xy.coords(1:NROW(cs$Env$xdata[subset]))
        cs$set_xlim(c(1,NROW(cs$Env$xdata[subset])))
        cs$Env$xstep <- 1
    } else {
        xycoords <- xy.coords(.index(cs$Env$xdata[cs$Env$xsubset]), 
                              cs$Env$xdata[cs$Env$xsubset][,1])
        cs$Env$xycoords <- xycoords
        cs$Env$xlim <- range(xycoords$x, na.rm=TRUE)
        cs$Env$xstep <- diff(xycoords$x[1:2])
        cs$set_xlim(cs$Env$xlim)
    }
    if(!is.null(FUN)){
        fun <- match.fun(FUN)
        .formals <- formals(fun)
        .formals <- modify.args(formals=.formals, arglist=list(...), dots=TRUE)
        if("R" %in% names(.formals)) .formals <- modify.args(formals=.formals, arglist=NULL, R=x, dots=TRUE)
        if("x" %in% names(.formals)) .formals <- modify.args(formals=.formals, arglist=NULL, x=x, dots=TRUE)
        .formals$... <- NULL
        R <- try(do.call(fun, .formals), silent=TRUE)
        if(inherits(R, "try-error")) { 
            message(paste("FUN function failed with message", R))
            cs$Env$R <- x
        } else {
            cs$Env$R <- R
        }
    } else {
        cs$Env$R <- x
    }
    
    if(is.null(ylim)){
        if(isTRUE(multi.panel)){
            if(yaxis.same){
                yrange <- range(cs$Env$R[subset], na.rm=TRUE)
                if(all(yrange == 0)) yrange <- yrange + c(-1,1)
                cs$set_ylim(list(structure(yrange,fixed=TRUE)))
            } else {
                yrange <- range(cs$Env$R[,1][subset], na.rm=TRUE)
                if(all(yrange == 0)) yrange <- yrange + c(-1,1)
                cs$set_ylim(list(structure(yrange,fixed=TRUE))) 
            }
        } else {
            yrange <- range(cs$Env$R[subset], na.rm=TRUE)
            if(all(yrange == 0)) yrange <- yrange + c(-1,1)
            cs$set_ylim(list(structure(yrange,fixed=TRUE)))
        }
        cs$Env$constant_ylim <- range(cs$Env$R[subset], na.rm=TRUE)
    } else {
        cs$set_ylim(list(structure(ylim, fixed=TRUE)))
        cs$Env$constant_ylim <- ylim
    }
    
    cs$set_frame(1,FALSE)
    cs$add(expression(atbt <- new.axTicksByTime2(xdata[xsubset]),
                      segments(xycoords$x[atbt], 
                               get_ylim()[[2]][1],
                               xycoords$x[atbt],
                               get_ylim()[[2]][2], 
                               col=theme$grid, lwd=grid.ticks.lwd, lty=grid.ticks.lty)),
           clip=FALSE,expr=TRUE)
    
    cs$add_frame(0,ylim=c(0,1),asp=0.5)
    cs$set_frame(1)
   
    cs$add(expression(if(NROW(xdata[xsubset])<400) 
    {axis(1,at=xycoords$x,labels=FALSE,col=theme$grid2,tcl=0.3)}),expr=TRUE)
    
    cs$add(expression(axt <- axTicksByTime(xdata[xsubset],format.labels=format.labels),
                      axis(1,
                           at=xycoords$x[axt], 
                           labels=names(axt), 
                           las=theme$xaxis.las, lwd.ticks=1, mgp=c(3,1.5,0), 
                           tcl=-0.4, cex.axis=theme$cex.axis)),
           expr=TRUE)
   
    text.exp <- c(expression(text(xlim[1],0.5,main,font=2,col='#444444',offset=0,cex=1.1,pos=4)),
                  expression(text(xlim[2],0.5,
                                  paste(start(xdata[xsubset]),end(xdata[xsubset]),sep=" / "),
                                  col=1,adj=c(0,0),pos=2)))
    cs$add(text.exp, env=cs$Env, expr=TRUE)
    
    cs$set_frame(2)
    cs$Env$y_grid_lines <- function(ylim) { 
        p <- pretty(ylim,5)
        p[p > ylim[1] & p < ylim[2]]
    }
    
    exp <- expression(segments(xlim[1], 
                               y_grid_lines(get_ylim()[[2]]), 
                               xlim[2], 
                               y_grid_lines(get_ylim()[[2]]), 
                               col=theme$grid, lwd=grid.ticks.lwd, lty=grid.ticks.lty))
    if(yaxis.left){
        exp <- c(exp, 
                 expression(text(xlim[1]-xstep*2/3-max(strwidth(y_grid_lines(get_ylim()[[2]]))), 
                                 y_grid_lines(get_ylim()[[2]]),
                                 noquote(format(y_grid_lines(get_ylim()[[2]]), justify="right")),
                                 col=theme$labels, srt=theme$srt, offset=0, pos=4, 
                                 cex=theme$cex.axis, xpd=TRUE)))
    }
    if(yaxis.right){
        exp <- c(exp, 
                 expression(text(xlim[2]+xstep*2/3,
                                 y_grid_lines(get_ylim()[[2]]),
                                 noquote(format(y_grid_lines(get_ylim()[[2]]), justify="right")),
                                 col=theme$labels, srt=theme$srt, offset=0, pos=4, 
                                 cex=theme$cex.axis, xpd=TRUE)))
    }
    cs$add(exp, env=cs$Env, expr=TRUE)
    cs$set_frame(2)
    if(isTRUE(multi.panel)){
        lenv <- new.env()
        lenv$xdata <- cs$Env$R[,1][subset]
        lenv$label <- colnames(cs$Env$R[,1])
        lenv$type <- cs$Env$type
        if(yaxis.same){
            lenv$ylim <- cs$Env$constant_ylim
        } else {
            lenv$ylim <- range(cs$Env$R[,1][subset], na.rm=TRUE)
        }
        exp <- expression(new.chart.lines(xdata, 
                                      type=type, 
                                      lty=lty,
                                      lwd=lwd,
                                      lend=lend,
                                      col=theme$col, 
                                      up.col=theme$up.col, 
                                      dn.col=theme$dn.col,
                                      legend.loc=legend.loc))
        cs$add(exp, env=c(lenv,cs$Env), expr=TRUE)
        text.exp <- expression(text(x=xycoords$x[2],
                                    y=ylim[2]*0.9,
                                    labels=label,
                                    adj=c(0,0),cex=1,offset=0,pos=4))
        cs$add(text.exp,env=c(lenv, cs$Env),expr=TRUE)
        
        if(NCOL(cs$Env$xdata) > 1){
            for(i in 2:NCOL(cs$Env$xdata)){
                lenv <- new.env()
                lenv$xdata <- cs$Env$R[,i][subset]
                lenv$label <- cs$Env$column_names[i]
                if(yaxis.same){
                    lenv$ylim <- cs$Env$constant_ylim
                } else {
                    yrange <- range(cs$Env$R[,i][subset], na.rm=TRUE)
                    if(all(yrange == 0)) yrange <- yrange + c(-1,1)
                    lenv$ylim <- yrange
                }
                lenv$type <- cs$Env$type
                cs$add_frame(ylim=c(0,1),asp=0.25)
                cs$next_frame()
                text.exp <- expression(text(x=xlim[1],
                                            y=0.5,
                                            labels="",
                                            adj=c(0,0),cex=0.9,offset=0,pos=4))
                cs$add(text.exp, env=c(lenv,cs$Env), expr=TRUE)
                cs$add_frame(ylim=lenv$ylim, asp=NCOL(cs$Env$xdata), fixed=TRUE)
                cs$next_frame()
                
                exp <- expression(new.chart.lines(xdata[xsubset], 
                                              type=type, 
                                              lty=lty,
                                              lwd=lwd,
                                              lend=lend,
                                              col=theme$col, 
                                              up.col=theme$up.col, 
                                              dn.col=theme$dn.col,
                                              legend.loc=legend.loc))
                lenv$y_grid_lines <- function(ylim) { 
                    p <- pretty(ylim,5)
                    p[p > ylim[1] & p < ylim[2]]
                }
                
                exp <- c(exp, 
                         expression(segments(xlim[1],
                                             y_grid_lines(ylim),
                                             xlim[2], 
                                             y_grid_lines(ylim), 
                                             col=theme$grid, lwd=grid.ticks.lwd, lty=grid.ticks.lty)),
                         expression(atbt <- new.axTicksByTime2(xdata[xsubset]),
                                    segments(xycoords$x[atbt], 
                                             ylim[1],
                                             xycoords$x[atbt], 
                                             ylim[2], 
                                             col=theme$grid, lwd=grid.ticks.lwd, lty=grid.ticks.lty)))
                if(yaxis.left){
                    exp <- c(exp, 
                             expression(text(xlim[1]-xstep*2/3-max(strwidth(y_grid_lines(ylim))), 
                                             y_grid_lines(ylim),
                                             noquote(format(y_grid_lines(ylim),justify="right")),
                                             col=theme$labels, srt=theme$srt, offset=0, 
                                             pos=4, cex=theme$cex.axis, xpd=TRUE)))
                }
                if(yaxis.right){
                    exp <- c(exp, 
                             expression(text(xlim[2]+xstep*2/3, y_grid_lines(ylim),
                                             noquote(format(y_grid_lines(ylim),justify="right")),
                                             col=theme$labels, srt=theme$srt, offset=0,
                                             pos=4, cex=theme$cex.axis, xpd=TRUE)))
                }
                cs$add(exp,env=c(lenv, cs$Env),expr=TRUE,no.update=TRUE)
                text.exp <- expression(text(x=xycoords$x[2],
                                            y=ylim[2]*0.9,
                                            labels=label,
                                            adj=c(0,0),cex=1,offset=0,pos=4))
                cs$add(text.exp,env=c(lenv, cs$Env),expr=TRUE)
            }
        }
    } else {
        if(type == "h" & NCOL(x) > 1) 
            warning("only the univariate series will be plotted")
        cs$add(expression(new.chart.lines(R[xsubset], 
                                      type=type, 
                                      lty=lty,
                                      lwd=lwd,
                                      lend=lend,
                                      col=theme$col,
                                      up.col=theme$up.col, 
                                      dn.col=theme$dn.col,
                                      legend.loc=legend.loc)),expr=TRUE)
        assign(".xts_chob", cs, .plotxtsEnv)
    }
    if(!is.null(panels) && nchar(panels) > 0) {
        panels <- parse(text=panels, srcfile=NULL)
        for( p in 1:length(panels)) {
            if(length(panels[p][[1]][-1]) > 0) {
                cs <- eval(panels[p])
            } else {
                cs <- eval(panels[p])
            }
        }
    }
    assign(".xts_chob", cs, .plotxtsEnv)
    cs
}

#https://github.com/cran/PerformanceAnalytics/blob/master/R/chart.TimeSeries.R

new.plot.xts(y, 
             yaxis.right = yaxis.right, 
             type = type, 
             lty = lty, 
             lwd = lwd,
             xaxis.las = las,
             main = colnames(y)[1],
             ylim = ylim,
             col = colorset,
             legend.loc = legend.loc,
             cex.axis = cex.axis,
             grid.col = grid.color,
             grid.ticks.lwd = 1,
             ...)
			 
}
