
# hex chart
output$hex_xSelection <- renderUI({.getxSelection(rv$subData)})
output$hex_ySelection <- renderUI({.getySelection(rv$subData)})
output$hex_colVarSelection <- renderUI({.getColVarSelection(rv$subData)})
output$hex_colPal <- renderUI({.getColPal()})

output$hexChart <- renderPlot({
    
#    .denseHexChart <- function(mainData, input){
        
        hexplot <- NULL
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
                
            cr <- colorRampPalette(myCol)
            hexplot <- hexbinplot(as.numeric(xData) ~ as.numeric(yData), xbins=20, xlab=xSelection, ylab=ySelection, colramp=cr, colorcut = seq(0, 1, length.out = n.levels))


                
            }
        }
        hexplot
#    }
#    .denseHexChart(mainData, input)
}) # end hexChart
