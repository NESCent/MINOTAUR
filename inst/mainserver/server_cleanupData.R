
# select filter variable
output$filterVariable <- renderUI({
    selectizeInput('filterVariable','Filter variable',choices=c('',names(mainData)), multiple=FALSE,
    options=list(
    placeholder='select, search or input variable names',
    selectOnTab=TRUE,
    create=FALSE,
    onInitialize = I('function() { this.setValue(""); }')
    )
    )
})

output$filterActiveDefault <- renderUI({
    radioButtons('filterActiveDefault',label=NULL,choices=c('select all','deselect all'),inline=TRUE)
})

# (simple well panel for messing around with)
output$filterOptions <- renderUI({
    wellPanel(
    h4('(scratch pad)'),
    p(reactiveValuesToList(RVselectedVariables))
    )
})

# create 'subset' panels dynamically
RVselectedVariables <- reactiveValues()
RVselectedValues <- reactiveValues()
observe({
    if (!is.null(input$filterVariable)) {
        if (!(input$filterVariable%in%reactiveValuesToList(RVselectedVariables)) & !(input$filterVariable=='')) {
            RVselectedVariables[[as.character(length(reactiveValuesToList(RVselectedVariables))+1)]] = input$filterVariable
        }
    }
})
output$subsetPanels <- renderUI({
    RVsvar_list <- reactiveValuesToList(RVselectedVariables)
    RVsvalue_list <- reactiveValuesToList(RVselectedValues)
    if (length(RVsvar_list)>0) {
        panelList = list()
        for (i in 1:length(RVsvar_list)) {
            value <- range(mainData[,names(mainData)==RVsvar_list[[i]]])
            if (length(RVsvar_list)==(length(RVsvalue_list)+1)) {
                RVselectedValues[[as.character(length(RVsvar_list))]] <- value
            } else {
                if (RVsvalue_list[[i]][1]!=value[1] | RVsvalue_list[[i]][2]!=value[2]) {
                    value <- RVsvalue_list[[i]]
                }
            }
            if (!is.null(RVsvar_list[[i]])) {
                if (i==length(RVsvar_list)) {
                    panelList[[i]] = wellPanel(
                    fluidRow(
                    column(8,
                    h5(RVsvar_list[[i]])
                    ),
                    column(4,
                    actionButton(paste('varClose',i,sep=''),label='reset')
                    )
                    ),
                    sliderInput(paste('varSlider',i,sep=''),label=NULL,min=min(mainData[,names(mainData)==RVsvar_list[[i]]]),max=max(mainData[,names(mainData)==RVsvar_list[[i]]]),value=value,round=TRUE)
                    ,style='padding: 10px')
                } else {
                    panelList[[i]] = wellPanel(
                    fluidRow(
                    column(9,
                    h5(RVsvar_list[[i]])
                    ),
                    column(3,
                    actionButton(paste('varClose',i,sep=''),label=NULL,icon=icon('close'))
                    )
                    ),
                    sliderInput(paste('varSlider',i,sep=''),label=NULL,min=min(mainData[,names(mainData)==RVsvar_list[[i]]]),max=max(mainData[,names(mainData)==RVsvar_list[[i]]]),value=value,round=TRUE)
                    ,style='padding: 10px')
                }
            }
        }
        rev(panelList)
    }
})
observe({
    RVsvar_list <- reactiveValuesToList(RVselectedVariables)
    if (length(RVsvar_list)>0) {
        for (i in 1:length(RVsvar_list)) {
            evalString <- paste('input$varSlider',i,sep='')
            evalString <- eval(parse(text=evalString))
            if (!is.null(evalString)) {
                RVselectedValues[[as.character(i)]] <- evalString
            }
        }
    }
})
observe({
    RVsvar_list <- reactiveValuesToList(RVselectedVariables)
    if (length(RVsvar_list)>0) {
        for (i in 1:length(RVsvar_list)) {
            evalString <- paste('input$varClose',i,sep='')
            evalString <- eval(parse(text=evalString))
            if (!(is.null(evalString))) {
                if (evalString==1) {
                    RVselectedVariables[[as.character(i)]] = NULL
                }
            }
        }
    }
})


# render data table
RVsubsetBoolean <- reactiveValues(sub=rep(1,nrow(mainData)))  # which rows in data frame are to be used (set to 1)
output$mainDataTable <- renderDataTable({
    RVsvar_list <- reactiveValuesToList(RVselectedVariables)
    RVsvalue_list <- reactiveValuesToList(RVselectedValues)
    RVsubsetBoolean$sub = rep(1,nrow(mainData))
    if (length(RVsvar_list)>0) {
        for (i in 1:length(RVsvar_list)) {
            values <- RVselectedValues[[as.character(i)]]
            #RVsubsetBoolean$sub = RVsubsetBoolean$sub*(mainData[,names(mainData)==RVsvar_list[[i]]]>0)
            #RVsubsetBoolean$sub = unlist(reactiveValuesToList(RVsubsetBoolean)$sub) * rep(10,nrow(mainData))
        }
    }
    processedData = data.frame(select=reactiveValuesToList(RVsubsetBoolean)$sub,mainData)
    processedData
},options=list(scrollX='300px', scrollY='400px', searching=FALSE))

