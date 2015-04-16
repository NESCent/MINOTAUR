
# circular Manhattan plot
output$selectplot_circleMH <- renderUI({
    selectizeInput('Choose plot','select statistic for plot',choices=c('',names(mainData)), multiple=FALSE,
    options=list(
    placeholder='bubble, manhattan or circle manhattan',
    selectOnTab=TRUE,
    create=FALSE,
    onInitialize = I('function() { this.setValue(""); }')
    )
    )
})

# circular Manhattan plot
output$circleMHTplot <- renderPlot({
    circosmht(mydata=mainData, pcut.outlier=0.002)
})
