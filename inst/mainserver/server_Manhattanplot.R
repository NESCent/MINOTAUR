
## linear Manhattan plot
output$select_linearMH <- renderUI({
    selectizeInput('Choose plot','select a variable',choices=c('',names(mainData)), multiple=FALSE,
    options=list(
    placeholder='choose a p value',
    selectOnTab=TRUE,
    create=FALSE,
    onInitialize = I('function() { this.setValue(""); }')
    )
    )
})

# linenar Manhattan Plot
output$LinearMHTplot <- renderPlot({
    mhtplot(mydata=mainData, pcut.outlier=0.002)
})
