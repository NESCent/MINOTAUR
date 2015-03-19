
# server.R (one of three scripts required to run any Rshiny app)

# Author: Bob Verity
# Date: 18/03/2015

# Purpose:
# The main workings of the app. This is where we use variables - often passed
# from the ui.R script - to create a series of different plots and outputs. We
# don't actually realise these plots here. Rather, we pass them back over the
# ui.R script, which dictates our layout etc.

#### ------------------------------------------------------------------

# load in the data (generate random data for now)
n = 1000
mainData = data.frame('norm Stat'=rnorm(n), 'Fst'=rbeta(n,5,5), 'diversity'=rgamma(n,5,5), 'group'=sample(3,n,rep=TRUE), 'S1'=rnorm(n), 'S2'=rnorm(n), 'S3'=rnorm(n), 'S4'=rnorm(n), 'S5'=rnorm(n), check.names=FALSE)

filterActive = data.frame(matrix(TRUE,1,ncol(mainData)))
names(filterActive)=names(mainData)

# Define the shiny server functionality
shinyServer(function(input, output) {
  
  rvs <- reactiveValues(filterVariable=NULL,filterActive=filterActive)
  
  # data for subsetting
  output$mainDataTable <- renderDataTable({
    mainData
  },options=list(scrollX='300px', scrollY='300px', searching=FALSE))
  
  # reactive uis for filtering variables
  output$filterVariable <- renderUI({
    checkboxInput('filterActive',label='included in data frame',value=unlist(reactiveValuesToList(rvs)$filterActive[input$filterVariable]))
  })
  output$filterActiveDefault <- renderUI({
    radioButtons('filterActiveDefault',label=NULL,choices=c('select all','deselect all'),inline=TRUE)
  })
  output$filterVariable <- renderUI({
    selectizeInput('filterVariable','Filter variable',choices=c('',names(mainData)), multiple=TRUE,
                   options=list(
                     placeholder='select, search or input items',
                     selectOnTab=TRUE,
                     create=FALSE,
                     onInitialize = I('function() { this.setValue(""); }')
                     )
                   )
  })
  output$filterActive <- renderUI({
    checkboxInput('filterActive',label='included in data frame',value=unlist(reactiveValuesToList(rvs)$filterActive[input$filterVariable]))
  })
  output$filterOptions <- renderUI({
    wellPanel(
      h4(rvs$filterVariable),
      p(reactiveValuesToList(rvs)$filterActive)
    )
  })
  observe({
#     if (!is.null(input$filterVariable) & input$filterVariable!='') {
#       if (is.null(rvs$filterVariable)) {
#         rvs$filterVariable = input$filterVariable
#       }
#       if (rvs$filterVariable==input$filterVariable) {
#         print('foobar')
#       } else {
#         print('diff')
#         
#       }
#       rvs$filterActive[input$filterVariable] = input$filterActive
#     }
  })
  
  # TEST histogram of random noise
  output$randomHist <- renderPlot({
    
    hist(rnorm(1000))
    
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