



#############
## plot_2D ##
#############

### Plot function for generating xy scatterplots.

########################################################################

###################
## DOCUMENTATION ##
###################

#' Generate x-y scatter plots.
#'
#' Generate x-y scatter plots with outliers overlayed.
#' Easily control the aesthetics of plot scatter and outlier points.
#'
#' @param x A numeric vector containing a variable to be plotted along the x-axis of the xy scatter.
#' @param y A numeric vector containing a variable to be plotted along the y-axis of the xy scatter.
#' @param xlab An optional character string specifying a label for the x-axis.
#' @param ylab An optional character string specifying a label for the y-axis.
#' @param xlim An optional numeric vector of length 2 specifying limits for the x-axis.
#' @param ylim An optional numeric vector of length 2 specifying limits for the y-axis.
#' @param n.bins An integer specifying the number of bins for the xy scatter pixels.
#' @param col.pal A character string specifying the color palette to be used for the xy scatter pixels.
#' @param grid A logical specifying whether to overlay a grid on top of the plotting area.
#' @param outlier.x An optional numeric vector giving the x-coordinates of an outlier to be plotted as points.
#' @param outlier.y An optional numeric vector (required if outlier.x is provided)
#' giving the y-coordinates of an outlier to be plotted as points.
#' @param outlier.col A character string specifying the color of the outline of the outlier points.
#' @param outlier.col.bg A character string specifying the background (fill) color of the outlier points.
#' @param outlier.transp A number between 0 and 1 specifying the degree of transparency to be added to the outlier points.
#' @param outlier.pch A number between 21 and 25 specifying the shape of the outlier points. (See ?par for details.)
#' @param outlier.cex A number specifying the cex (size) of the outlier points.
#'
#' @author Caitlin Collins \email{caitiecollins@@gmail.com}
#' @author Kathleen Lotterhos \email{k.lotterhos@@neu.edu}
#'
#' @import ash fields
#'
#' @export

## TO DO: Add @examples content

########################################################################

plot_2D<- function(x, y,
                   xlab=NULL, ylab=NULL,
                   xlim=NULL, ylim=NULL,
                   n.bins=100,
                   col.pal="heat.colors", grid=FALSE,
                   outlier.x=NULL, outlier.y=NULL,
                   outlier.col="blue", outlier.col.bg="purple",
                   outlier.transp=0.25,
                   outlier.pch=24, outlier.cex=1.5){

  require(ash)
  require(fields)

  x_sub <- outlier.x
  y_sub <- outlier.y

  if(outlier.transp != 0){
    outlier.transp <- 1 - outlier.transp
    outlier.col <- transp(outlier.col, alpha = outlier.transp)
    outlier.col.bg <- transp(outlier.col.bg, alpha = outlier.transp)
  }

  data1 <- cbind(x, y)
  data1b <- data1[complete.cases(data1),]

  if(length(xlim)==0){
    xlim_up <- max(x, na.rm=TRUE)
    xlim_lower <- min(x, na.rm=TRUE)
  }

  if(length(ylim)==0){
    ylim_up <- max(y, na.rm=TRUE)
    ylim_lower <- min(y, na.rm=TRUE)
  }

  binned <- bin2(data1b,
                 matrix(c(xlim_lower,xlim_up,ylim_lower,ylim_up), 2,2, byrow=TRUE),
                 nbin=c(n.bins,n.bins))
  binned$nc[binned$nc==0]=NA

  ## SCATTER PLOT
  image.plot(seq(xlim_lower,xlim_up,length.out = n.bins),
             seq(ylim_lower,ylim_up, length.out=n.bins),
             binned$nc,
             xlab=xlab, ylab=ylab, add=FALSE, col=col.pal)

  ## ADD OUTLIER POINTS
  points(x_sub, y_sub, pch=outlier.pch, cex=outlier.cex, col=outlier.col, bg=outlier.col.bg)

  ## ADD GRID
  if(grid) grid()

  ## SET TITLE TO VALUE BEING PLOTTED
  ## NOTE: to be changed to textInput( w x- and ySelection selected)!!!!!!
  if(!is.null(xlab) & !is.null(ylab)) title(paste(xlab, "vs.", ylab, sep=" "))
} # end plot_2D


