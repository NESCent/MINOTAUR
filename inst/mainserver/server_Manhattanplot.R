
## linear Manhattan plot
output$selectplot_linearMH <- renderUI({
    selectizeInput('Choose plot','select statistic for plot',choices=c('',names(mytoysdata)), multiple=FALSE,
    options=list(
    placeholder='bubble, manhattan or circle manhattan',
    selectOnTab=TRUE,
    create=FALSE,
    onInitialize = I('function() { this.setValue(""); }')
    )
    )
})

# linenar Manhattan Plot
output$LinearMHTplot <- renderPlot({
    mhtplot(mydata=mytoysdata, pcut.outlier=0.002)
})
