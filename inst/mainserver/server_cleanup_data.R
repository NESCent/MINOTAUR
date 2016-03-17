

#######################
## CLEANUP DATA PAGE ##  ------------------------------------------------------------------------------------
#######################

######################
## Box: Subset Data ##
######################

# box for subsetting data
output$box_subsetData <- renderUI({
  box(title="Subset Data", status="primary", solidHeader=TRUE, collapsible=TRUE, width=12,
      h3('Subset Columns and Remove Missing Data'),
      p('Here you can clean up your data before taking it forward to the next steps.',strong('Do not'),'manually remove outliers at this stage - these will be dealt with later.'),

      h3('Remove Columns'),

      selectizeInput('selectSubsetCols', label=NULL, choices=names(rawData()$data), multiple=TRUE),

      radioButtons('subsetRemoveOrRetain', label=NULL, choices=list('remove chosen columns'='remove', 'retain chosen columns'='retain')),

      h3('Remove Missing Data'),

      checkboxGroupInput('removeMissing', label=NULL, choices=list('Remove NA'='NA', 'Remove NaN'='NaN', 'Remove non-finite\n(Inf and -Inf)'='non-finite'))
  )
})

# reactive conductor for subsetting raw data. Returns list(data,rows,cols,numRemoved,percentRemoved)
finalData <- reactive({
  nullOutput <- list(data=NULL, rows=NULL, cols=NULL, numRemoved=NULL, percentRemoved=NULL)
  rawData <- rawData()
  print(paste('SUBSETTING DATA',sample(100,1)))

  # if rawData$data is NULL, return nullOutput
  if (is.null(rawData$data))
    return(nullOutput)

  # if remove-or-retain radio button is NULL (ie. on startup), return nullOutput
  if (is.null(input$subsetRemoveOrRetain))
    return(nullOutput)

  # subset columns
  if (input$subsetRemoveOrRetain=='remove') {
    subData <- rawData$data[,which(!names(rawData$data)%in%input$selectSubsetCols),drop=FALSE]
  } else if (input$subsetRemoveOrRetain=='retain') {
    subData <- rawData$data[,which(names(rawData$data)%in%input$selectSubsetCols),drop=FALSE]
  }
  if (ncol(subData)==0)  # if all columns removed
    return(nullOutput)

  # subset rows
  if (!is.null(input$removeMissing)) {
    # remove NA
    if ('NA'%in%input$removeMissing) {
      subData <- subData[rowSums(mapply(is.na,subData))==0,,drop=FALSE]
      if (nrow(subData)==0)  # if all rows removed
        return(nullOutput)
    }
    # remove NaN
    if ('NaN'%in%input$removeMissing) {
      subData <- subData[which(rowSums(mapply(function(x){x=='NaN'},subData),na.rm=T)==0),,drop=FALSE]
      if (nrow(subData)==0)  # if all rows removed
        return(nullOutput)
    }
    # remove non-finite (Inf and -Inf)
    if ('non-finite'%in%input$removeMissing) {
      subData <- subData[which(rowSums(mapply(function(x){x%in%c('Inf','-Inf')},subData),na.rm=T)==0),,drop=FALSE]
      if (nrow(subData)==0)  # if all rows removed
        return(nullOutput)
    }
  }

  # finally return subData
  output <- list(data=subData,
                 rows=nrow(subData),
                 cols=ncol(subData),
                 numRemoved=rawData$rows-nrow(subData),
                 percentRemoved=round((1-nrow(subData)/rawData$rows))*100, digits=10)
  return(output)
})

#################################
## Box: Missing Data % Removed ##
#################################

# valueBox for % missing data removed
output$valueBox_missingDataRemoved <- renderUI({
  valueBox(value=HTML(paste('<font size=5>rows removed:  </font> <font size=6>',
                            finalData()$numRemoved,
                            ' (',
                            finalData()$percentRemoved,
                            '%)</font>'
                            ,sep='')
  ), subtitle='', color='yellow', width=12)
})

########################
## Box: Filtered Data ##
########################

# box for showing filtered data
output$box_cleanupData <- renderUI({
  box(title="Filtered Data", status="warning", solidHeader=TRUE, collapsible=TRUE, width=12,
      dataTableOutput("cleanupData")
  )
})

# filtered data table
output$cleanupData <- renderDataTable({
  finalData()$data
},options=list(scrollX=TRUE, scrollY='500px'), rownames=FALSE,
)
