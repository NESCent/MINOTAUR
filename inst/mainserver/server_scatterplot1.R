#

# scatterplot
output$scatter_xSelection <- renderUI({.getxSelection(rv$subData)})
output$scatter_ySelection <- renderUI({.getySelection(rv$subData)})
output$scatter_colVarSelection <- renderUI({.getColVarSelection(rv$subData)})
output$scatter_colPal <- renderUI({.getColPal()})

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

