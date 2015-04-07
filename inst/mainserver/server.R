
# right-aligned numeric text box with adjustable width
# warning - this box can contain non-numeric strings. Check for these using suppressWarnings(!is.na(as.numeric(val)))
bobText1 <- function(inputId, label, value="", size='10px', placeholder='', style='text-align:right',...) {
  div(style="display:inline-block",
    tags$label(label, `for` = inputId), 
    tags$input(id=inputId, type="text", pattern='[0-9]{1,99}|.[0-9]{1,99}|[0-9]{1,99}.[0-9]{1,99}|inf|Inf|INF|-inf|-Inf|-INF', value=value, size=size, placeholder=placeholder, style=style,...)
  )
}

# nice looking close button
bobCloseButton <- function(inputId) {
  div(
    class='close',
    tags$button(id=inputId, type="button", class="btn action-button", style='font-size:25px; width:15px; height:25px; text-align:center; line-height:10px', HTML('<i class="close"></i>&#10006'))
  )
}

# nice looking reset button
bobResetButton <- function(inputId) {
  div(
    style='float:right',
    tags$button(id=inputId, type="button", class="btn action-button btn-primary", style='font-size:15px; width:50px; line-height:12px; text-align:center', HTML('<i class="icon-star"></i>reset'))
  )
}

# violin-style plot
bobViolinPlot <- function(x,subMin=0,subMax=0) {
  x = x[!is.na(x)]
  par(mar=c(0,0,0,0))
  d = density(x,from=min(x),to=max(x))
  d$y = d$y/max(d$y)
  rangeMin = (min(x)+max(x))/2 - 1.5*(max(x)-min(x))/2
  rangeMax = (min(x)+max(x))/2 + 1.5*(max(x)-min(x))/2
  plot(0,type='n',xlim=c(rangeMin,rangeMax),ylim=c(-1,1),axes=FALSE,xlab='',ylab='')
  polygon(c(d$x,rev(d$x)),c(d$y,-rev(d$y)),col=grey(0.9),border=NA)
  if (subMin!=subMax) {
    s1 = min(subMin,subMax)
    s2 = max(subMin,subMax)
    s1 = max(s1,min(x))
    s2 = min(s2,max(x))
    z1 = which(abs(d$x-s1)==min(abs(d$x-s1)))[1]
    z2 = which(abs(d$x-s2)==min(abs(d$x-s2)))[1]
    polygon(c(d$x[z1:z2],rev(d$x[z1:z2])),c(d$y[z1:z2],-rev(d$y[z1:z2])),col='#ff9000',border=NA)
    abline(v=c(d$x[z1],d$x[z2]),col='#ff9000',lty=2)
  }
  polygon(c(d$x,rev(d$x)),c(d$y,-rev(d$y)))
  text(min(x),0,signif(min(x),digits=3),pos=2)
  text(max(x),0,signif(max(x),digits=3),pos=4)
}


# bar sub plot
bobBarSubplot <- function(tab,selected,total) {
  # restrict to small-ish number of levels
  if (length(tab)<50) {
    par(mar=c(0,0,0,0))
    z = as.numeric(names(tab))
    colVec = rep(grey(0.9),length(tab))
    colVec[selected] = '#ff9000'
    barplot(tab/max(tab),col=colVec,space=0,axes=FALSE,xlab='',ylab='',ylim=c(-0.5,1.2))
    text(min(z)-0.5,0,min(z),pos=1)
    text(max(z)-0.5,0,max(z),pos=1)
  
  # otherwise produce simple summary box
  } else {
    par(mar=c(0,0,0,0))
    plot(0,type='n',axes=FALSE,xlab='',ylab='',xlim=c(0,1),ylim=c(0,1))
    myText = paste(total-length(selected),'of',total,'levels removed')
    text(0,0.5,myText,pos=4)
  }
}



##############
## Server.R ##
##############

# This is the MINOTAUR server.
# This function calls all of the functions running behind our app.

# temporarily loading all required packages here
require("shiny")
require("rCharts")
require("rHighcharts")
require("stats4")
require("adegenet")
require("MASS")
require("RColorBrewer")
require("ggplot2")
require("scales")
require("hexbin")

## temporarily sourcing .R files here
source("mhtCirclePlot.R")
source("mhtplot.R")
source("uiFunctions.R")

#### ------------------------------------------------------------------

# load in the data (use mytoys.txt data for now)
mainData <- read.table("mytoys.txt",head=T)


# Define the shiny server functionality
shinyServer(function(input, output, session) {
  
  # Cleanup data
  source("server_cleanupData.R", local=T)
  
  # Scatterplot
  source("server_scatterplot1.R", local=T)
  
  # Hex plot 1 (plain R)
  source("server_hexplot1.R", local=T)
  
  # Hex plot 2 (ggplot)
  source("server_hexplot2.R", local=T)
  
  # Linear Manhattan plot
  source("server_Manhattanplot.R", local=T)
  
  # Circular Manhattan plot
  source("server_circleplot.R", local=T)
  
  # (example summary table)
  output$summaryTable <- renderTable({
    summary(mainData)
  })
  
})
