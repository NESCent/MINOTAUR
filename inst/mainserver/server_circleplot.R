
# circular Manhattan plot
output$selectplot_circleMH <- renderUI({
    selectizeInput('Choose plot','select statistic for plot',choices=c('',names(mytoysdata)), multiple=FALSE,
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
    circosmht(mydata=mytoysdata, pcut.outlier=0.002)
})
