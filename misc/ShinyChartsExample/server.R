
# server.R (one of three scripts required to run any Rshiny app)

# Author: Bob Verity
# Date: 02/04/2014

# Purpose:
# The main workings of the app. This is where we use variables - often passed from the ui.R script - to create a series of different plots and outputs. We don't actually realise these plots here. Rather, we pass them back over the ui.R script, which dictates our layout etc.

#### ------------------------------------------------------------------

# Anything that is implemented here (before we implement the function shinyServer()) is basically ordinary R code. We can define variables here, and can also read in files.
# Lets create some random data...
randomValues1 = rnorm(50)
randomValues2 = rgamma(50,5,5)
randomValues3 = rbeta(50,2,2)
groupValues = c(rep("Group1",25),rep("Group2",25))
myDataFrame = data.frame(Normal=randomValues1,Gamma=randomValues2,Beta=randomValues3,group=groupValues)

# Define the shiny server functionality
shinyServer(function(input, output) {

# Our first output will be an ordinary R plot
output$plot1 <- renderPlot({
	
	# Obtain data selected by user
	xData = myDataFrame[,names(myDataFrame)==input$xSelection]
	yData = myDataFrame[,names(myDataFrame)==input$ySelection]
	remainingVariable = which(names(myDataFrame)!=input$xSelection & names(myDataFrame)!=input$ySelection)[1]
	sizeData = myDataFrame[,remainingVariable]
	
	# Process data to create interaction
	xDataProcessed = round(xData + input$interactionStrength*xData*yData*sizeData, digits=2)
	yDataProcessed = round(yData + input$interactionStrength*xData*yData*sizeData, digits=2)
	sizeDataProcessed = round(sizeData + input$interactionStrength*xData*yData*sizeData, digits=2)
	outputData = data.frame(Selection1=xDataProcessed,Selection2=yDataProcessed,Size=sizeDataProcessed,group=myDataFrame$group)
	
		# histogram of y-axis data
		hist(outputData$Selection2, breaks=20, col=8, xlab=paste(input$ySelection,"data"), main="Marginal distribution in second variable")
	
})
	
# Our second output will be a table of summary results
output$table1 <- renderTable({
	
	# Obtain data selected by user
	xData = myDataFrame[,names(myDataFrame)==input$xSelection]
	yData = myDataFrame[,names(myDataFrame)==input$ySelection]
	remainingVariable = which(names(myDataFrame)!=input$xSelection & names(myDataFrame)!=input$ySelection)[1]
	sizeData = myDataFrame[,remainingVariable]
	
	# Process data to create interaction
	xDataProcessed = round(xData + input$interactionStrength*xData*yData*sizeData, digits=2)
	yDataProcessed = round(yData + input$interactionStrength*xData*yData*sizeData, digits=2)
	sizeDataProcessed = round(sizeData + input$interactionStrength*xData*yData*sizeData, digits=2)
	outputData = data.frame(Selection1=xDataProcessed,Selection2=yDataProcessed,Size=sizeDataProcessed,group=myDataFrame$group)
	
	# Output summary table of simulated data
	summary(outputData)
	
})

# Our third output will be a bubble plot, produced using rCharts (rendered with 'Highcharts')
output$bubbleChart1 <- renderChart2({

	# Obtain data selected by user
	xData = myDataFrame[,names(myDataFrame)==input$xSelection]
	yData = myDataFrame[,names(myDataFrame)==input$ySelection]
	remainingVariable = which(names(myDataFrame)!=input$xSelection & names(myDataFrame)!=input$ySelection)[1]
	sizeData = myDataFrame[,remainingVariable]
	
	# Process data to create interaction
	xDataProcessed = round(xData + input$interactionStrength*xData*yData*sizeData, digits=2)
	yDataProcessed = round(yData + input$interactionStrength*xData*yData*sizeData, digits=2)
	sizeDataProcessed = round(sizeData + input$interactionStrength*xData*yData*sizeData, digits=2)
	outputData = data.frame(Selection1=xDataProcessed,Selection2=yDataProcessed,Size=sizeDataProcessed,group=myDataFrame$group)

	# Create a bubble plot
	bubbles <- hPlot(Selection2 ~ Selection1, data=outputData, type = "bubble", title = "Bubble demo", subtitle = "drag a box to zoom in on a particular area", size = "Size", group = "group")
	bubbles$chart(zoomType = "xy")
	
	# Remember to return the plot object (this will be passed to the gui)
	return(bubbles)
    
})

})
