#' A New chart.RollingPerformance Function
#' 
#' This function allows you to plot rolling performance metrics and is based on xts::plot.xts rather than base graphics
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param width number of periods to apply rolling function window over
#' @param FUN any function that can be evaluated using a single set of returns
#' @param fill a three-component vector or list (recycled otherwise) providing 
#' filling values at the left/within/to the right of the data range. See the 
#' fill argument of \code{\link{na.fill}} for details.
#' @param main set the chart title, same as in \code{\link{plot}}
#' @param ylim set the y-axis limit, same as in \code{\link{plot}}
#' @param \dots any other passthru parameters to \code{\link{plot}} or the
#' function specified
#' @keywords new.chart.RollingPerformance
#' @examples 
#' library(xtsPerAnl)
#' library(xts)  #library(xtsExtra)
#' library(PerformanceAnalytics)
#' data(managers)
#' new.chart.RollingPerformance(managers)
#' @export

new.chart.RollingPerformance <- function (R, width = 12, FUN = "Return.annualized", ...,  ylim = NULL, main = NULL, fill = NA)
{ 
    x = checkData(R)
    
    # Get dimensions and labels
    columns = ncol(x)
    columnnames = colnames(x)
    
    # Separate function args from plot args
    dotargs <-list(...)
    funargsmatch = pmatch(names(dotargs), names(formals(FUN)), nomatch = 0L)
    funargs = dotargs[funargsmatch>0L]
    if(is.null(funargs))funargs=list()
    funargs$...=NULL
    
    plotargs = dotargs[funargsmatch==0L]
    plotargs$...=NULL
    if (!length(plotargs)) plotargs=list()
    
    funargs$width=width
    funargs$FUN=FUN
    funargs$fill = fill
    funargs$align='right'
    
    # Calculate
    for(column in 1:columns) {
        # the drop=FALSE flag is essential for when the zoo object only has one column
        rollargs<-c(list(data=na.omit(x[,column,drop=FALSE])),funargs)
        column.Return.calc <- do.call(rollapply,rollargs)
        if(column == 1)
            Return.calc = xts(column.Return.calc)
        else
            Return.calc = merge(Return.calc,column.Return.calc)
    }
    if(is.null(ylim)){
        ylim = c(min(0,min(Return.calc, na.rm=TRUE)),max(Return.calc, na.rm=TRUE))
    }    
    colnames(Return.calc) = columnnames
    
    if(is.null(main)){
        
        freq = periodicity(R)
        
        switch(freq$scale,
               minute = {freq.lab = "minute"},
               hourly = {freq.lab = "hour"},
               daily = {freq.lab = "day"},
               weekly = {freq.lab = "week"},
               monthly = {freq.lab = "month"},
               quarterly = {freq.lab = "quarter"},
               yearly = {freq.lab = "year"}
        )
        
        main = paste(columnnames[1], " Rolling ",width,"-",freq.lab," ", FUN,sep="")
    }
    
    
    plotargs$R=Return.calc
    plotargs$main=main
    plotargs$ylim=ylim
    # Here new.chart.TimeSeries() function is used which is based on plot.xts rather than base graphics
    do.call(new.chart.TimeSeries,plotargs)
}
