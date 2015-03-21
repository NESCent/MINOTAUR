

##############
## Server.R ##
##############

# This is the MINOTAUR server.
# This function calls all of the functions running behind our app.
# The main workings of the app. This is where we use variables - often passed from the ui.R script - to create a series of different plots and outputs. 
# We don't actually realise these plots here. Rather, we pass them back over the ui.R script, which dictates our layout etc.



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

# load in the data (generate random data for now)
n = 1000
mainData = data.frame('normstat'=rnorm(n), 'Fst'=rbeta(n,5,5), 'diversity'=rgamma(n,5,5), 'bob'=sample(3,n,rep=TRUE), 'S1'=rnorm(n), 'S2'=rnorm(n), 'S3'=rnorm(n), 'S4'=rnorm(n), 'S5'=rnorm(n), check.names=FALSE)
mainData = round(mainData,digits=3)

#mytoysdata <- read.table(paste(dirname(shinyPath),"/mytoys.txt",sep=""),head=T,sep="\t")
mytoysdata <- read.table("mytoys.txt",head=T,sep="\t")

#create dataframe with from matrix with 9 columns of TRUE in a single row
filterActive = data.frame(matrix(TRUE,1,ncol(mainData)))
names(filterActive)=names(mainData)

# Define the shiny server functionality
shinyServer(function(input, output) {
  
  ###################
  ## UI FUNCTIONS! ##
  ###################
  
  # Data for Dropdown menu 
  output$ySelection <- renderUI({.getySelection(mainData)})
  
  output$xSelection <- renderUI({.getxSelection(mainData)})
  
  output$reactiveui3 <- renderUI({
    if(!is.null(mainData)){
      selectInput(inputId = "colorSelection",
                  label = "Choose axis to color by:",
                  choices = names(mainData),
                  selected = names(mainData)[5])
    }
  })
  
  output$col.pal <- renderUI({.getColPal()})
  
  
  # data for subsetting
  RVsubsetBoolean <- reactiveValues(sub=rep(1,nrow(mainData)))
  output$mainDataTable <- renderDataTable({
    RVsvar_list <- reactiveValuesToList(RVselectedVariables)
    RVsvalue_list <- reactiveValuesToList(RVselectedValues)
    RVsubsetBoolean$sub = rep(1,nrow(mainData))
    if (length(RVsvar_list)>0) {
      for (i in 1:length(RVsvar_list)) {
        values <- RVselectedValues[[as.character(i)]]
        #RVsubsetBoolean$sub = RVsubsetBoolean$sub*(mainData[,names(mainData)==RVsvar_list[[i]]]>0)
        #RVsubsetBoolean$sub = unlist(reactiveValuesToList(RVsubsetBoolean)$sub) * rep(10,nrow(mainData))
      }
    }
    processedData = data.frame(select=reactiveValuesToList(RVsubsetBoolean)$sub,mainData)
    processedData
  },options=list(scrollX='300px', scrollY='400px', searching=FALSE))
  
  # select filter variable
  output$filterVariable <- renderUI({
    selectizeInput('filterVariable','Filter variable',choices=c('',names(mainData)), multiple=FALSE,
                   options=list(
                     placeholder='select, search or input items',
                     selectOnTab=TRUE,
                     create=FALSE,
                     onInitialize = I('function() { this.setValue(""); }')
                   )
    )
  })
  output$filterActiveDefault <- renderUI({
    radioButtons('filterActiveDefault',label=NULL,choices=c('select all','deselect all'),inline=TRUE)
  })
  
  # (simple well panel for messing around with)
  output$filterOptions <- renderUI({
    wellPanel(
      h4('header'),
      p(reactiveValuesToList(RVselectedVariables))
    )
  })

  # create subset panels dynamically
  RVselectedVariables <- reactiveValues()
  RVselectedValues <- reactiveValues()
  observe({
    if (!is.null(input$filterVariable)) {
      if (!(input$filterVariable%in%reactiveValuesToList(RVselectedVariables)) & !(input$filterVariable=='')) {
        RVselectedVariables[[as.character(length(reactiveValuesToList(RVselectedVariables))+1)]] = input$filterVariable
      }
    }
  })
  output$subsetPanels <- renderUI({
    RVsvar_list <- reactiveValuesToList(RVselectedVariables)
    RVsvalue_list <- reactiveValuesToList(RVselectedValues)
    if (length(RVsvar_list)>0) {
      panelList = list()
      for (i in 1:length(RVsvar_list)) {
        value <- range(mainData[,names(mainData)==RVsvar_list[[i]]])
        if (length(RVsvar_list)==(length(RVsvalue_list)+1)) {
          RVselectedValues[[as.character(length(RVsvar_list))]] <- value
        } else {
          if (RVsvalue_list[[i]][1]!=value[1] | RVsvalue_list[[i]][2]!=value[2]) {
            value <- RVsvalue_list[[i]]
          }
        }
        if (!is.null(RVsvar_list[[i]])) {
          if (i==length(RVsvar_list)) {
            panelList[[i]] = wellPanel(
              fluidRow(
                column(8,
                       h5(RVsvar_list[[i]])
                ),
                column(4,
                       actionButton(paste('varClose',i,sep=''),label='reset')
                )
              ),
              sliderInput(paste('varSlider',i,sep=''),label=NULL,min=min(mainData[,names(mainData)==RVsvar_list[[i]]]),max=max(mainData[,names(mainData)==RVsvar_list[[i]]]),value=value,round=TRUE)
            ,style='padding: 10px')
          } else {
            panelList[[i]] = wellPanel(
              fluidRow(
                column(9,
                  h5(RVsvar_list[[i]])
                ),
                column(3,
                  actionButton(paste('varClose',i,sep=''),label=NULL,icon=icon('close'))
                )
              ),
              sliderInput(paste('varSlider',i,sep=''),label=NULL,min=min(mainData[,names(mainData)==RVsvar_list[[i]]]),max=max(mainData[,names(mainData)==RVsvar_list[[i]]]),value=value,round=TRUE)
            ,style='padding: 10px')
          }
        }
      }
      rev(panelList)
    }
  })
  observe({
    RVsvar_list <- reactiveValuesToList(RVselectedVariables)
    if (length(RVsvar_list)>0) {
      for (i in 1:length(RVsvar_list)) {
        evalString <- paste('input$varSlider',i,sep='')
        evalString <- eval(parse(text=evalString))
        if (!is.null(evalString)) {
          RVselectedValues[[as.character(i)]] <- evalString
        }
      }
    }
  })
  observe({
    RVsvar_list <- reactiveValuesToList(RVselectedVariables)
    if (length(RVsvar_list)>0) {
      for (i in 1:length(RVsvar_list)) {
        evalString <- paste('input$varClose',i,sep='')
        evalString <- eval(parse(text=evalString))
        if (!(is.null(evalString))) {
          if (evalString==1) {
            RVselectedVariables[[as.character(i)]] = NULL
          }
        }
      }
    }
  })

  
  #################
  ## MAKE PLOTS! ##
  #################

  output$bubbleChart1 <- renderPlot({

    .makeBubbles <- function(mainData, input){
      
      bubbles <- NULL
      
      xSelection <- input$xSelection
      ySelection <- input$ySelection
      col <- input$colorSelection
      colPal <- input$col.pal
      
      if(!is.null(mainData)){
        if(!is.null(ySelection) && !is.null(ySelection)){
          
          # Obtain data selected by user
          xData = mainData[,names(mainData)==xSelection]
          yData = mainData[,names(mainData)==ySelection]
          colDat = mainData[,names(mainData)==col] 
          
          outputData = data.frame(xData,yData)
          
          get.levels <- levels(as.factor(colDat))
          n.levels <- length(get.levels)
          myCol <- get(colPal)(n.levels)
          scheme <- colDat
          scheme <- as.numeric(as.factor(scheme))
          myCol <- myCol[scheme]
          
          bubbles <- plot(outputData[,2] ~ outputData[,1], 
                          data=outputData, xlab=xSelection, ylab=ySelection, 
                          col=myCol, bty = "l", pch = 20)
          
        } 
      }
      return(bubbles)
    }
    .makeBubbles(mainData, input)
  }) # end bubbleChart1

  output$hexChart <- renderPlot({
    
    .denseHexChart <- function(mainData, input){
      
      hexplot <- NULL
      
      xSelection <- input$xSelection
      ySelection <- input$ySelection
      
      if(!is.null(mainData)){
        if(!is.null(ySelection) && !is.null(ySelection)){
          
          # Obtain data selected by user
          xData = mainData[,names(mainData)==xSelection]
          yData = mainData[,names(mainData)==ySelection]
          
          outputData = data.frame(xData,yData)
          
          hexplot <- plot(hexbin(outputData[,2] ~ outputData[,1]))
          
        } 
      }
      return(hexplot)
    }
    .denseHexChart(mainData, input)
  }) # end denseHexChart

  output$ggHexChart <- renderPlot({
    
    .ggHexChart <- function(mainData, input){
      
      gghexplot <- NULL
      
      xSelection <- input$xSelection
      ySelection <- input$ySelection
      
      if(!is.null(mainData)){
        if(!is.null(ySelection) && !is.null(ySelection)){
          
          # Obtain data selected by user
          xData = mainData[,names(mainData)==xSelection]
          yData = mainData[,names(mainData)==ySelection]
          
          outputData = data.frame(xData,yData)
          
          gghexplot <- qplot(outputData[,2], outputData[,1]) + stat_binhex()
          
        } 
      }
      return(gghexplot)
    }
    .ggHexChart(mainData, input)
  }) # end ggHexChart  
  
  output$contourChart <- renderPlot({
    
    .contourChart <- function(mainData, input){
      
      contourPlot <- NULL
      
      xSelection <- input$xSelection
      ySelection <- input$ySelection
      
      k <- 11
      my.cols <- rev(brewer.pal(k, "RdYlBu"))
      
      if(!is.null(mainData)){
        if(!is.null(ySelection) && !is.null(ySelection)){
          
          # Obtain data selected by user
          xData = mainData[,names(mainData)==xSelection]
          yData = mainData[,names(mainData)==ySelection]
          
          outputData = data.frame(xData,yData)
          z <- kde2d(outputData[,1], outputData[,2], n=100)
          contourPlot <- plot(outputData[,2] ~ outputData[,1], 
                              data=outputData, xlab=xSelection, ylab=ySelection, 
                              pch=19, cex=0.4)
          contour(z, drawlabels=FALSE, nlevels=k, col=my.cols, add=TRUE, lwd=2)
          
        } 
      }
      return(contourPlot)
    }
    .contourChart(mainData, input)
  }) # end contourChart   
  
  # TEST histogram of random noise
  output$randomHist <- renderPlot({
    
    hist(rnorm(1000))
    
  }) # end randomHist
  
  
  ## add options for plotting
  output$selectplot_linearMH <- renderUI({
    selectizeInput('Choose plot','select statistic for plot',choices=c('',names(mytoysdata)), multiple=FALSE,
                   options=list(
                     placeholder='bubble, manhattan or circle manhattan',
                     selectOnTab=TRUE,
                     create=FALSE,
                     onInitialize = I('function() { this.setValue(""); }')
                   )
    )
  })
  output$selectplot_circleMH <- renderUI({
    selectizeInput('Choose plot','select statistic for plot',choices=c('',names(mytoysdata)), multiple=FALSE,
                   options=list(
                     placeholder='bubble, manhattan or circle manhattan',
                     selectOnTab=TRUE,
                     create=FALSE,
                     onInitialize = I('function() { this.setValue(""); }')
                   )
    )
  })
  output$circleMHTplot <- renderPlot({
    circosmht(mydata=mytoysdata, pcut.outlier=0.002)
  })
  
  # TEST Linenar Manhattan Plot
  output$LinearMHTplot <- renderPlot({
    mhtplot(mydata=mytoysdata, pcut.outlier=0.002)
  })
  # TEST summary table
  output$summaryTable <- renderTable({
    
    summary(mainData)
    
  })
  
  # main scatterplot
  output$test1 <- renderChart2({
    
    testPlot <- hPlot(diversity ~ Fst, data=mainData, type = "bubble", title = "Plot Title", subtitle = "here is some subtitle", size = "diversity", group = "group")
    return(testPlot)
    
  })
  
})
