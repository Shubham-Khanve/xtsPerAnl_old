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
#' library(xtsExtra)  #library(xts)
#' library(PerformanceAnalytics)
#' data(managers)
#' new.chart.TimeSeries(managers)

new.chart.TimeSeries <- function(R, 
                                 auto.grid = TRUE,
                                 yaxis.right = FALSE, 
                                 type = "l", 
                                 lty = 1, 
                                 lwd = 2, 
                                 las = par("las"),
                                 main = NULL,
                                 ylab=NULL, 
                                 date.format.in="%Y-%m-%d",
                                 date.format = NULL,
                                 xlim = NULL,
                                 ylim = NULL, 
                                 element.color="darkgray",
                                 colorset = (1:12), 
                                 pch = (1:12),
                                 legend.loc = NULL,
                                 ylog = FALSE, 
                                 cex.axis=0.8, 
                                 major.ticks = "auto",
                                 grid.color="lightgray",
                                 grid.lty = "dotted",
                                 xaxis.labels = NULL, 
                                 ...){
    
    y = checkData(R)    
    
    plot.xts(y, 
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

