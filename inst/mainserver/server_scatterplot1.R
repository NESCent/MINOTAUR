

##############
## .plot_2D ##
##############

### Plot function, need to move to function area

## QUESTION: DO WE WANT TO RELEASE plot_2D FOR
## PUBLIC USE AS A STAND-ALONE FUNCTION????????????????????????????????????????????????????
## (If so, we need to (1) remove the (.) from
## the front of the fn name and in every instance of its use in the app,
## and (2) document the fn using roxygen2-style comments
## s.t it is added to the namespace and Rd/man folder etc.)

.plot_2D<- function(x,y, xlab, ylab, xlim=NULL, ylim=NULL, nbin,
                   x_sub, y_sub){

  #if(!("ash" %in% installed.packages())){install.packages("ash")}
  require(ash)
  require(fields)

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
             xlab=xlab, ylab=ylab, add=FALSE, col=grey.colors(75, 0.25,0.9))
    points(x_sub, y_sub, pch=24, cex=1.5, col="dodgerblue", bg="darkorange")
} # end .plot_2D





######################
## .getScatterplot1 ##
######################
.getScatterplot1 <- function(input){

    scatterplot <- NULL
    xSelection <- input$xSelection
    ySelection <- input$ySelection
    logx <- input$scatter_Checkbox_x
    #print(logx)
      if(logx=="none"){logx=NULL}else{
     if(sum(xSelection<0)>0){print("Error: You are trying to log-transform negative values in the X variable. These values will not be plotted.")}
      if(logx=="log2"){logx=2}
      if(logx=="log10"){logx=10}
      }
    logy <- input$scatter_Checkbox_y
    if(logy=="none"){logy=NULL}else{
      if(sum(ySelection<0)>0){print("Error: You are trying to log-transform negative values in the Y variable. These values will not be plotted.")}
      if(logy=="log2"){logy=2}
      if(logy=="log10"){logy=10}
      }
    flipX <- input$scatter_Checkbox_x_flip
      if(flipX=="No"){flipX=1}else{
      if(flipX=="Yes"){flipX=-1}}
    flipY <- input$scatter_Checkbox_y_flip
      if(flipY=="No"){flipY=1}else{
      if(flipY=="Yes"){flipY=-1}}
    nbins <- as.numeric(as.character(input$scatter_nbins))
    colVar <- input$colVarSelection #this is the 3rd variable
    cutoff <- as.numeric(input$scatter_cutoff)
    tail <- input$scatter_cutoff_tail
   # colPal <- input$colPal


    if(!is.null(rv$subData)){
        if(!is.null(xSelection) && !is.null(ySelection)){

            # Obtain data selected by user
            xData = rv$subData[,names(rv$subData)==xSelection]
            yData = rv$subData[,names(rv$subData)==ySelection]
            colData = rv$subData[,names(rv$subData)==colVar]

            if(length(logx)==1){xData=log(xData+1e-40, logx)}
            if(length(logy)==1){yData=log(yData+1e-40, logy)}

            print(c(1,cutoff))
            if(is.na(cutoff)){cutoff=0.01}
            if(tail=="Upper"){
                cutoff=(1-cutoff)
            }
            print(c(2,cutoff))
            colDataNoNA <- colData[!is.na(colData)]
            colDataNew <- rank(colDataNoNA)/length(colDataNoNA)
            colDataNew2 <- colData
            colDataNew2[!is.na(colData)] <- colDataNew

            if(tail=="Lower"){
             xData_sub <- xData[colDataNew2<=cutoff]
             yData_sub <- yData[colDataNew2<=cutoff]
            }
            if(tail=="Upper"){
             xData_sub <- xData[colDataNew2>=cutoff]
             yData_sub <- yData[colDataNew2>=cutoff]
            }

            xData <- xData*flipX
            yData <- yData*flipY
            xData_sub <- xData_sub*flipX
            yData_sub <- yData_sub*flipY
            #print(cbind(xData_sub,yData_sub))
            #print(logy)
            #print(c(min(yData,na.rm=TRUE), max(yData, na.rm=TRUE)))

            # get colors
  #          get.levels <- levels(as.factor(colData))
  #          n.levels <- length(get.levels)
  #          colIndex <- as.numeric(as.factor(colData))
 #           if(!(colPal=="black")){
#            myCol <- get(colPal)(n.levels)[colIndex]
#            }else(myCol <- rgb(0,0,0,0.2))

            # produce plot
            #scatterplot <- plot(xData, yData, xlab=xSelection, ylab=ySelection, col=myCol, pch=20)
              scatterplot <- .plot_2D(xData, yData, xlab=xSelection, ylab=ySelection,
                                     nbin=nbins, x_sub=xData_sub, y_sub=yData_sub)

        }
    }
    scatterplot
} # end .getScatterplot1



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



##########################
## .getScatterDataTable ##
##########################
.getScatterDataTable <- function(input, mainData){
  if(!is.null(mainData)){
    colVar <- input$colVarSelection
    if(!is.null(colVar)){
      #print("colVar")
      #print(colVar)
      colData = mainData[,names(mainData)==colVar]
      cutoff <- as.numeric(input$scatter_cutoff)
      tail <- input$scatter_cutoff_tail
      #print(head(colData))
            if(is.na(cutoff)){cutoff=0.01}
            if(tail=="Upper"){
                cutoff=(1-cutoff)
            }
            colDataNoNA <- colData[!is.na(colData)]
            colDataNew <- rank(colDataNoNA)/length(colDataNoNA)
            colDataNew2 <- colData
            colDataNew2[!is.na(colData)] <- colDataNew

       if(tail=="Lower"){
      indexes <- which(colDataNew2 <= cutoff)
       }
      if(tail=="Upper"){
       indexes <- which(colDataNew2 >= cutoff)
      }
      print(c("indexes",length(indexes)))
      a <- mainData[indexes,]
    }
  }
} # end .getScatterDataTable

