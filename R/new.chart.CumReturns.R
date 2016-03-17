#' A New chart.CumReturns Function
#' 
#' This function allows you to plot periodic returns with cumulation and is based on xts::plot.xts rather than base graphics
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param wealth.index if \code{wealth.index} is \code{TRUE}, shows the "value
#' of $1", starting the cumulation of returns at 1 rather than zero
#' @param geometric utilize geometric chaining (TRUE) or simple/arithmetic chaining (FALSE) to aggregate returns,
#' default TRUE
#' @param legend.loc places a legend into one of nine locations on the chart:
#' bottomright, bottom, bottomleft, left, topleft, top, topright, right, or
#' center.
#' @param colorset color palette to use, set by default to rational choices
#' @param begin Align shorter series to: \itemize{ \item first - prior value of
#' the first column given for the reference or longer series or, \item axis -
#' the initial value (1 or zero) of the axis.  }
#' @param \dots any other passthru parameters
#' @keywords new.chart.CumReturns
#' @examples
#' library(xtsPerAnl)
#' library(PerformanceAnalytics)
#' library(xts)  #library(xtsExtra)
#' data(managers)
#' new.chart.CumReturns(managers)
#' @export

new.chart.CumReturns <-
    function (R, wealth.index = FALSE, geometric = TRUE, legend.loc = NULL, colorset = (1:12), begin = c("first","axis"), ...)
    {
        na.skip <- function (x, FUN=NULL, ...) 
        {   
            nx <- na.omit(x)
            fx <- FUN(nx, ... = ...)
            if (is.vector(fx)) {
                result <- .xts(fx, .index(x), .indexCLASS = indexClass(x))
            }
            else {
                result <- merge(fx, .xts(, .index(x)))
            }
            return(result)
        }
        
        #source= https://r-forge.r-project.org/scm/viewvc.php/pkg/PerformanceAnalytics/R/na.skip.R?view=markup&root=returnanalytics
        
        begin = begin[1]
        x = checkData(R)
        
        columns = ncol(x)
        columnnames = colnames(x)
        
        one = 0
        if(!wealth.index)
            one = 1
  
        if(begin == "first") {
            length.column.one = length(x[,1])
            start.row = 1
            start.index = 0
            while(is.na(x[start.row,1])){
                start.row = start.row + 1
            }
            x = x[start.row:length.column.one,]
            if(geometric)
                reference.index = na.skip(x[,1],FUN=function(x) {cumprod(1+x)})
            else
                reference.index = na.skip(x[,1],FUN=function(x) {cumsum(x)})
        }
        for(column in 1:columns) {
            if(begin == "axis") {
                start.index = FALSE
            } else {
                start.row = 1
                while(is.na(x[start.row,column])){
                    start.row = start.row + 1
                }
                start.index=ifelse(start.row > 1,TRUE,FALSE)
            }
            if(start.index){
                if(geometric)
                    z = na.skip(x[,column],FUN = function(x,index=reference.index[(start.row - 1)]) {rbind(index,1+x)})
                else
                    z = na.skip(x[,column],FUN = function(x,index=reference.index[(start.row - 1)]) {rbind(1+index,1+x)})
            } else {
                z = 1+x[,column] 
            }
            column.Return.cumulative = na.skip(z,FUN = function(x, one, geometric) {if(geometric) cumprod(x)-one else (1-one) + cumsum(x-1)},one=one, geometric=geometric)
            if(column == 1)
                Return.cumulative = column.Return.cumulative
            else
                Return.cumulative = merge(Return.cumulative,column.Return.cumulative)
        }
        if(columns == 1)
            Return.cumulative = as.xts(Return.cumulative)
        colnames(Return.cumulative) = columnnames
        
        # Here new.chart.TimeSeries() function is used which is based on plot.xts rather than base graphics
        new.chart.TimeSeries(Return.cumulative, colorset = colorset, legend.loc = legend.loc, ...)
    }

