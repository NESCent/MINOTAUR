

# scatterplot
output$scatter_xSelection <- renderUI({.getxSelection(mainData)})
output$scatter_ySelection <- renderUI({.getySelection(mainData)})
output$scatter_colVarSelection <- renderUI({.getColVarSelection(mainData)})
output$scatter_colPal <- renderUI({.getColPal()})

output$scatterplot1 <- renderPlot({
    
    scatterplot <- NULL
    xSelection <- input$xSelection
    ySelection <- input$ySelection
    colVar <- input$colVarSelection
    colPal <- input$colPal
    
    if(!is.null(mainData)){
        if(!is.null(ySelection) && !is.null(ySelection)){
            
            # Obtain data selected by user
            xData = mainData[,names(mainData)==xSelection]
            yData = mainData[,names(mainData)==ySelection]
            colData = mainData[,names(mainData)==colVar]
            
            # get colors
            get.levels <- levels(as.factor(colData))
            n.levels <- length(get.levels)
            colIndex <- as.numeric(as.factor(colData))
            myCol <- get(colPal)(n.levels)[colIndex]
            
            # produce plot
            scatterplot <- plot(xData, yData, xlab=xSelection, ylab=ySelection, col=myCol, pch=20)
        }
    }
    scatterplot
}) # end scatterplot1

