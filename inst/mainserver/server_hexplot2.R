
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
