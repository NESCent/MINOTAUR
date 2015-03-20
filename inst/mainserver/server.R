

##############
## Server.R ##
##############

# This is the MANIPULATE server.
# This function calls all of the functions running behind our app.
# The main workings of the app. This is where we use variables - often passed from the ui.R script - to create a series of different plots and outputs. 
# We don't actually realise these plots here. Rather, we pass them back over the ui.R script, which dictates our layout etc.



# Load packages
require("shiny")
require("rCharts")
require("rHighcharts")
require("stats4")
require("adegenet")



#### ------------------------------------------------------------------

# load in the data (generate random data for now)
n = 1000
mainData = data.frame('normstat'=rnorm(n), 'Fst'=rbeta(n,5,5), 'diversity'=rgamma(n,5,5), 'bob'=sample(3,n,rep=TRUE), 'S1'=rnorm(n), 'S2'=rnorm(n), 'S3'=rnorm(n), 'S4'=rnorm(n), 'S5'=rnorm(n), check.names=FALSE)
mainData = round(mainData,digits=3)

#mytoysdata <- read.table(paste(dirname(shinyPath),"/mytoys.txt",sep=""),head=T,sep="\t")
mytoysdata <- read.table("mytoys.txt",head=T,sep="\t")

filterActive = data.frame(matrix(TRUE,1,ncol(mainData)))
names(filterActive)=names(mainData)

# Define the shiny server functionality
shinyServer(function(input, output) {
  
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
  
  # TEST histogram of random noise
  output$randomHist <- renderPlot({
    
    hist(rnorm(1000))
    
  })
  
  
  ## add options for plotting
  output$selectplot <- renderUI({
    selectizeInput('Choose plot','select statistic for plot',choices=c('',names(mytoysdata)), multiple=FALSE,
                   options=list(
                     placeholder='bubble, manhattan or circle manhattan',
                     selectOnTab=TRUE,
                     create=FALSE,
                     onInitialize = I('function() { this.setValue(""); }')
                   )
    )
  })
  # TEST circle Manhattan Plot
  output$circleMHTplot <- renderPlot({
    source("mhtCirclePlot.R")
    circosmht(mydata=mytoysdata, pcut.outlier=0.002)
  })
  
  # TEST Linenar Manhattan Plot
  output$LinearMHTplot <- renderPlot({
    source("mhtplot.R")
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
