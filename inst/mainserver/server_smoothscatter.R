

# smoothscatter plot
output$smooth_xSelection <- renderUI({.getxSelection(rv$subData)})
output$smooth_ySelection <- renderUI({.getySelection(rv$subData)})
output$smooth_colVarSelection <- renderUI({.getColVarSelection(rv$subData)})
output$smooth_colPal <- renderUI({.getColPal()})

output$smoothscatter <- renderPlot({
  
  smooth <- NULL
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
      myCol <- get(colPal)(n.levels)[colIndex]
      
      # produce plot
      smooth <- smoothScatter(xData ~ yData, xlab=xSelection, ylab=ySelection) # should integrate color options
    }
  }
  smooth
}) # end smoothscatter plot


