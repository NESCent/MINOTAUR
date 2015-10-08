
# circular Manhattan plot
setwd("~/MINOTAUR//inst/mainserver")
output$select_circleMH <- renderUI({.getySelection(rv$subData)})
output$smooth_colVarSelection <- renderUI({.getColVarSelection(rv$subData)})
output$smooth_colPal <- renderUI({.getColPal()})

output$selectplot_circleMH <- renderUI({
    selectizeInput('Choose plot','select variables for plot',choices=c('',names(mainData)), multiple=FALSE,
    options=list(
    placeholder='circle manhattan',
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
