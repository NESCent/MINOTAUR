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
  plot_2D<- function(x,y, xlab, ylab, xlim=NULL, ylim=NULL, nbin,
                     x_sub, y_sub){
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
    image.plot(seq(xlim_lower,xlim_up,length.out = nbin), seq(ylim_lower,ylim_up, length.out=nbin),
               binned$nc,
             xlab=xlab, ylab=ylab, add=FALSE, col=grey.colors(75, 0.1,0.9))
    points(x_sub, y_sub, pch=8, col="blue")
  }

### Render plot area

output$scatterplot1 <- renderPlot({
    
    scatterplot <- NULL
    xSelection <- input$xSelection
    ySelection <- input$ySelection
    logx <- as.numeric(as.character(input$scatter_Checkbox_x))
    logy <- as.numeric(as.character(input$scatter_Checkbox_y))
    nbins <- as.numeric(as.character(input$scatter_nbins))
    colVar <- input$colVarSelection
    cutoff <- as.numeric(input$scatter_cutoff)
    colPal <- input$colPal

    
    if(!is.null(rv$subData)){
        if(!is.null(xSelection) && !is.null(ySelection)){
            
            # Obtain data selected by user
            xData = rv$subData[,names(rv$subData)==xSelection]
            yData = rv$subData[,names(rv$subData)==ySelection]
            colData = rv$subData[,names(rv$subData)==colVar]
            
            if(length(logx)==1){xData=log(xData+1e-40, logx)}
            if(length(logy)==1){yData=log(yData+1e-40, logy)}
            print(cutoff)
            if(is.na(cutoff)){cutoff=colData[rank(colData)==round(length(colData)*0.01,0)]
              }
            #cutoff=0.01
            #print(cutoff)
            xData_sub <- xData[colData<cutoff]
            yData_sub <- yData[colData<cutoff]
            #print(cbind(xData_sub,yData_sub))
            #print(logy)
            #print(c(min(yData,na.rm=TRUE), max(yData, na.rm=TRUE)))
            
            # get colors
            get.levels <- levels(as.factor(colData))
            n.levels <- length(get.levels)
            colIndex <- as.numeric(as.factor(colData))
            if(!(colPal=="black")){
            myCol <- get(colPal)(n.levels)[colIndex]
            }else(myCol <- rgb(0,0,0,0.2))
            
            # produce plot
            #scatterplot <- plot(xData, yData, xlab=xSelection, ylab=ySelection, col=myCol, pch=20)
              scatterplot <- plot_2D(xData, yData, xlab=xSelection, ylab=ySelection, 
                                     nbin=nbins, x_sub=xData_sub, y_sub=yData_sub)

        }
    }
    scatterplot

}) # end scatterplot1

#    ####################
#       ## .fetchMyViolin ##
#       ####################
#       .fetchMyViolin <- function(x, input){
#         out <- NULL
#         evalString <- paste("input$continuousValue", col.num, sep="")
#         slider <- eval(parse(text=evalString))  
#         minSelected <- slider[1]
#         maxSelected <- slider[2]
#         #if(minSelected != -Inf || maxSelected != Inf){
#           out <- bobViolinPlot(x, minSelected, maxSelected)
#         #}
#         return(out)
#       } # end .fetchMyViolin
#         #}
# 
# output$violin <- renderPlot({
#   
# })
  

#######################
## render data table ##
#######################
    
    output$scatterDataTable <- renderDataTable({
          colVar <- input$colVarSelection
          print("colVar")
          print(colVar)
          colData = rv$subData[,names(rv$subData)==colVar]
          print(head(colData))
          cutoff <- as.numeric(input$scatter_cutoff)
          if(is.na(cutoff)){cutoff=colData[rank(colData)==round(length(colData)*0.01,0)]
              }
          print(cutoff)
          a<-rv$subData[colData < cutoff,]
          a
                },
              options=list(scrollX='300px', scrollY='400px', searching=FALSE)
              )
    

