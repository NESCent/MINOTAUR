

## NOTE: cleanup data fns (eg. bobCloseButton have been moved to uiFunctions.R)


##############
## Server.R ##
##############

# This is the MINOTAUR server.
# This function calls all of the functions running behind our app.

#### ------------------------------------------------------------------

# load in the data (use mytoys.txt data for now)
# mainData <- read.table("mytoys.txt",head=T)


#################   ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###
## shinyServer ##   ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###
#################   ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###

# Define the shiny server functionality
shinyServer(function(input, output, session) {

  ##########################################
  ## (temporarily) sourcing .R files here ##
  ##########################################

  # data
  source("data.R", local=T)

  # Cleanup data
  source("server_cleanupData.R", local=T)

  # Scatterplot
  source("server_scatterplot1.R", local=T)

  # Linear Manhattan plot
  source("server_Manhattanplot.R", local=T)

  # Circular Manhattan plot
  source("server_circleplot.R", local=T)

  # UI Functions
  source("uiFunctions.R", local=T)

  # Utils
  source("utils.R", local=T)

  ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###

  #####################
  ## INPUT DATA PAGE ##    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###
  #####################

  ##############
  ## get data ##
  ##############
  # NOTE: may want 2 fns- one for original data; one for subset data
  .get.data <- reactive({
    .read.input.data(input)
  })

  ###############################
  ## get summary of input data ##
  ###############################
  output$inputSummary <- renderPrint({
    if(is.null(.get.data())){
      out <- cat("\nNo data available to summarise.
                 Browse files to upload by clicking 'Choose file' at left.\n")
    }else{
      summary(.get.data())
    }
  }) # end inputSummary

  #############################
  ## (example summary table) ##
  #############################
  output$summaryTable <- renderTable({
    summary(.get.data())
  })

  ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###

  ########################
  ## CLEAN-UP DATA PAGE ##    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###
  ########################

  ################################################################
  ## create single list of reactive values to handle subsetting ##
  ################################################################

  ## create empty rv list
  rv <- reactiveValues()

  ## define values in observer to allow for data input changes
  observe({.getReactiveValues(.get.data())}) # end of reactiveValues

  ######################################
  ## 'update' and 'save' data buttons ##
  ######################################
  output$updateButton <- renderUI({.getUpdateSaveButtons()})

  ##############################
  ## on update button pressed ##
  ##############################
  updatePressed <- eventReactive(input$updateDataButton, {
    .getWidgetValues(.get.data())
    .updateData(input)
  })

  observe({
    updatePressed()
  })

  #####################################################
  ## selectize for choosing which columns are active ##
  #####################################################
  output$selectColumns <- renderUI({.getSelectColumns(.get.data())})

  ################################################
  ## dropdown menu for choosing filter variable ##
  ################################################
  output$filterVariable <- renderUI({.getFilterVariable(.get.data())})

  ##############################
  ## on new variable selected ##
  ##############################
  filterVariableSelected <- eventReactive(input$filterVariable, {
    .getFilterVariableSelected(.get.data())
  })

  observe({
    filterVariableSelected()
  })

  ########################################
  ## create current active subset panel ##
  ########################################
  output$subsetPanels_current <- renderUI({.getCurrentPanels(.get.data(),
                                                             rv)})

  ######################################
  ## create 'locked in' subset panels ##
  ######################################
  output$subsetPanels_locked <- renderUI({.getLockedPanels(rv,
                                                           i,
                                                           input,
                                                           .get.data())})
  #############################
  ## on reset button pressed ##
  #############################
  observe({.observeResetButton(i, input, rv, .get.data())})


  ##############################
  ## on close buttons pressed ##
  ##############################
  observe({.observeCloseButton(.get.data(),
                               rv,
                               input)})


  #######################
  ## render data table ##
  #######################

  ## TO DO: get table to show NAs (istead of empty cells)

  output$mainDataTable <- renderDataTable({
    rv$subData
  },
  options=list(scrollX='300px', scrollY='400px', searching=FALSE))


  ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###

  ######################
  ## SCATTERPLOT PAGE ##    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###
  ######################

  ########################
  ## render ui elements ##
  ########################
  # scatterplot
  output$scatter_xSelection <- renderUI({.getxSelection(rv$subData)})
  output$scatter_ySelection <- renderUI({.getySelection(rv$subData)})

  ## This is the 3rd variable that will color points by:
  output$scatter_colVarSelection <- renderUI({.getColVarSelection(rv$subData)})
  #output$scatter_colPal <- renderUI({.getColPal()})

  ######################
  ## render plot area ##
  ######################
  output$scatterplot1 <- renderPlot({ .getScatterplot1(input)}) # end scatterplot1

  #######################
  ## render data table ##
  #######################
  #print("hell")
  output$scatterDataTable <- renderDataTable({.getScatterDataTable(input, rv$subData)},
                                             options=list(scrollX='300px', scrollY='400px',
                                                          searching=FALSE))# end data Table

  ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###

  ##################################
  ## MANHATTAN PLOT PAGE (linear) ##    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###
  ##################################

  #####################
  ## get UI elements ##
  #####################
  output$linearMH_y1Selection <- renderUI({.getMHTySelection(rv$subData)})

  output$linearMH_xchr <- renderUI({.getMHTxChrSelection(rv$subData)})

  output$linearMH_xcood <- renderUI({.getMHTxPosSelection(rv$subData)})

  output$linearMH_p2Selection <- renderUI({.getMHTpSelection(rv$subData)})

  ###########################
  ## Linear Manhattan Plot ##
  ###########################
  output$LinearMHTplot <- renderPlot({.getLinearMHTPlot(rv$subData, input)})

  #######################
  ## render data table ##
  #######################
  #print("hell")
  output$ManhattanDataTable <- renderDataTable({.getManhattanDataTable(input, rv$subData)},
                                             options=list(scrollX='300px', scrollY='400px',
                                                          searching=FALSE))# end data Table

  ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###

  ######################
  ## CIRCLE PLOT PAGE ##    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###
  ######################

  #####################
  ## get UI elements ##
  #####################
  output$circle1mh <- renderUI({.getOuterCircleVar(rv$subData)})

  output$circle2mh <- renderUI({.getInnerCircleVar(rv$subData)})

  output$circle_xchr <- renderUI({.getChromosomeVar(rv$subData)})

  output$circle_xcood <- renderUI({.getCoordVar(rv$subData)})

  ########################
  ## get Manhattan plot ##
  ########################
  output$circleMHTplot <- renderPlot({.getCircleMHTPlot(rv$subData)})

  ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###

}) # end server
