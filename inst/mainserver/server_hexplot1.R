
# hex chart
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
}) # end hexChart
