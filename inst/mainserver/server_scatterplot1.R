#

# scatterplot
output$scatter_xSelection <- renderUI({.getxSelection(rv$subData)})
output$scatter_ySelection <- renderUI({.getySelection(rv$subData)})
output$scatter_colVarSelection <- renderUI({.getColVarSelection(rv$subData)})
output$scatter_colPal <- renderUI({.getColPal()})

### Plot function, need to move to function area
  if(!("ash" %in% installed.packages())){install.packages("ash")}
  library(ash)
  library(fields)
  plot_2D<- function(x,y, xlab, ylab, xlim=NULL, ylim=NULL, nbin){
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
                 nbin=c(nbin,nbin))
    binned$nc[binned$nc==0]=NA
    image.plot(seq(xlim_lower,xlim_up,length.out = nbin), seq(ylim_lower,ylim_up, length.out=nbin),binned$nc,
             xlab=xlab, ylab=ylab, add=FALSE, col=tim.colors(75))
  }

### Render plot area

output$scatterplot1 <- renderPlot({
    
    scatterplot <- NULL
    xSelection <- input$xSelection
    ySelection <- input$ySelection
    logx <- input$scatter_Checkbox_x
    logy <- input$scatter_Checkbox_y
    nbins <- input$scatter_nbins
    colVar <- input$colVarSelection
    cutoff <- input$scatter_cutoff
    colPal <- input$colPal

    
    if(!is.null(rv$subData)){
        if(!is.null(ySelection) && !is.null(ySelection)){
            
            # Obtain data selected by user
            xData = rv$subData[,names(rv$subData)==xSelection]
            yData = rv$subData[,names(rv$subData)==ySelection]
            colData = rv$subData[,names(rv$subData)==colVar]
            
            # get colors
            get.levels <- levels(as.factor(colData))
            n.levels <- length(get.levels)
            colIndex <- as.numeric(as.factor(colData))
            if(!(colPal=="black")){
            myCol <- get(colPal)(n.levels)[colIndex]
            }else(myCol <- rgb(0,0,0,0.2))
            
            # produce plot
            #scatterplot <- plot(xData, yData, xlab=xSelection, ylab=ySelection, col=myCol, pch=20)
            scatterplot <- plot_2D(xData, yData, xlab=xSelection, ylab=ySelection, nbin=50)
        }
    }
    scatterplot
}) # end scatterplot1

