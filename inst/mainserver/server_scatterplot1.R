#

# scatterplot
output$scatter_xSelection <- renderUI({.getxSelection(rv$subData)})
output$scatter_ySelection <- renderUI({.getySelection(rv$subData)})
output$scatter_colVarSelection <- renderUI({.getColVarSelection(rv$subData)})
output$scatter_colPal <- renderUI({.getColPal()})

### Plot function, need to move to function area
  plot_2D<- function(x,y, xlab, ylab, xlim=NULL, ylim=NULL){
  data1 <- cbind(x, y)
  data1b <- data1[complete.cases(data1),]
  if(length(xlim)==0){xlim=max(abs(x)*1.01, na.rm=TRUE)}
  if(length(ylim)==0){ylim=max(abs(y)*1.01, na.rm=TRUE)}  
        binned <- bin2(data1b, 
                 matrix(c(-xlim,xlim,-ylim,ylim), 2,2, byrow=TRUE), 
                 nbin=c(100,100))
    binned$nc[binned$nc==0]=NA
    image(seq(-xlim,xlim,length.out = 100), seq(-ylim,ylim, length.out=100),binned$nc,
             xlab=xlab, ylab=ylab, add=FALSE, col=tim.colors(75))
  }

output$scatterplot1 <- renderPlot({
    
    scatterplot <- NULL
    xSelection <- input$xSelection
    ySelection <- input$ySelection
    colVar <- input$colVarSelection
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
            scatterplot <- plot(xData, yData, xlab=xSelection, ylab=ySelection, col=myCol, pch=20)
        }
    }
    scatterplot
}) # end scatterplot1

