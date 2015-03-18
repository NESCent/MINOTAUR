
# server.R (one of three scripts required to run any Rshiny app)

# Author: Bob Verity
# Date: 17/03/2015

# Purpose:
# The main workings of the MANIPULATE visualisation app. This is where we use variables - often passed from the ui.R script - to create a series of different plots and outputs. We don't actually realise these plots here. Rather, we pass them back over the ui.R script, which dictates our layout etc.

#### ------------------------------------------------------------------

# load in the data (generate random data for now)
n = 100
mainData = data.frame('normStat'=rnorm(n), 'Fst'=rbeta(n,5,5), 'diversity'=rgamma(n,5,5), 'group'=sample(3,n,rep=TRUE))


# Define the shiny server functionality
shinyServer(function(input, output) {

# Our first output will be an ordinary R histogram
output$plot1 <- renderPlot({
# 	
# 	# Obtain data selected by user
# 	xData = mainData[,names(mainData)==input$xSelection]
# 	yData = mainData[,names(mainData)==input$ySelection]
# 	remainingVariable = which(names(mainData)!=input$xSelection & names(mainData)!=input$ySelection)[1]
# 	sizeData = mainData[,remainingVariable]
# 	
# 	# Process data to create interaction
# 	xDataProcessed = round(xData + input$interactionStrength*xData*yData*sizeData, digits=2)
# 	yDataProcessed = round(yData + input$interactionStrength*xData*yData*sizeData, digits=2)
# 	sizeDataProcessed = round(sizeData + input$interactionStrength*xData*yData*sizeData, digits=2)
# 	outputData = data.frame(Selection1=xDataProcessed,Selection2=yDataProcessed,Size=sizeDataProcessed,group=mainData$group)
# 	
	# histogram of y-axis data
	#hist(outputData$Selection2, breaks=20, col=8, xlab=paste(input$ySelection,"data"), main="Marginal distribution in second variable")
  hist(rnorm(1000))
	
})
	
# Our second output will be a table of summary results
output$table1 <- renderTable({
	
	# Obtain data selected by user
	xData = mainData[,names(mainData)==input$xSelection]
	yData = mainData[,names(mainData)==input$ySelection]
	remainingVariable = which(names(mainData)!=input$xSelection & names(mainData)!=input$ySelection)[1]
	sizeData = mainData[,remainingVariable]
	
	# Process data to create interaction
	xDataProcessed = round(xData + input$interactionStrength*xData*yData*sizeData, digits=2)
	yDataProcessed = round(yData + input$interactionStrength*xData*yData*sizeData, digits=2)
	sizeDataProcessed = round(sizeData + input$interactionStrength*xData*yData*sizeData, digits=2)
	outputData = data.frame(Selection1=xDataProcessed,Selection2=yDataProcessed,Size=sizeDataProcessed,group=mainData$group)
	
	# Output summary table of simulated data
	summary(outputData)
	
})

# Our third output will be a bubble plot, produced using rCharts (rendered with 'Highcharts')
output$bubbleChart1 <- renderChart2({

	# Obtain data selected by user
	xData = mainData[,names(mainData)==input$xSelection]
	yData = mainData[,names(mainData)==input$ySelection]
	remainingVariable = which(names(mainData)!=input$xSelection & names(mainData)!=input$ySelection)[1]
	sizeData = mainData[,remainingVariable]
	
	# Process data to create interaction
	xDataProcessed = round(xData + input$interactionStrength*xData*yData*sizeData, digits=2)
	yDataProcessed = round(yData + input$interactionStrength*xData*yData*sizeData, digits=2)
	sizeDataProcessed = round(sizeData + input$interactionStrength*xData*yData*sizeData, digits=2)
	outputData = data.frame(Selection1=xDataProcessed,Selection2=yDataProcessed,Size=sizeDataProcessed,group=mainData$group)

	# Create a bubble plot
	bubbles <- hPlot(Selection2 ~ Selection1, data=outputData, type = "bubble", title = "Bubble demo", subtitle = "drag a box to zoom in on a particular area", size = "Size", group = "group")
	bubbles$chart(zoomType = "xy")
	
	# Remember to return the plot object (this will be passed to the gui)
	return(bubbles)
    
})


output$test1 <- renderChart2({
  
  testPlot <- hPlot(normStat ~ Fst, data=mainData, type = "line", inverted=TRUE, title = "Bubble demo", subtitle = "drag a box to zoom in on a particular area", size = "diversity", group = "group")
  return(testPlot)
  
})


output$table2 <- renderDataTable({
  mainData
})

})
