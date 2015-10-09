
## linear Manhattan plot
output$select_linearMH <- renderUI({
    selectizeInput('yaxis','select y-axis variable',choices=names(rv$subData), multiple=FALSE,
                   selected = "Trait1_Beta")
})

# linenar Manhattan Plot
output$LinearMHTplot <- renderPlot({
    #yname = NULL
    yname = input$yaxis
   
    print(yname)
    if(!is.null(rv$subData) && !is.null(yname)){
      mhtplot(mydata=rv$subData, pcut.outlier=0.002, statistic.plot = yname)
    }
})
