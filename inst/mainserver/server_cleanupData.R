
# create single list of reactive values to handle subsetting
rv <- reactiveValues(
  
  selectColumnOn = rep(TRUE,ncol(mainData)),
  panelOrder = NULL,
  
  class = mapply(class,mainData),
  closeButtonPressed = rep(0,ncol(mainData)),
  continuousMin = rep(-Inf,ncol(mainData)),
  continuousMax = rep(Inf,ncol(mainData)),
  missingValues = replicate(ncol(mainData),NULL),
  integerSelected = replicate(ncol(mainData),NULL),
  factorSelected = replicate(ncol(mainData),NULL),
  modified = rep(FALSE,ncol(mainData)),
  
  subData = mainData
)  # end of reactiveValues


# (simple scratch pad for messing around with)
output$scratchPad <- renderUI({
  wellPanel(
    h4('(scratch pad)'),
    p(rv$closeButtonPressed)
  )
})

# ----------------------------------------------------------------------------------------------------

# set reactive values based on values in widgets
getWidgetValues <- function() {
  
  # search over all panels
  if (length(rv$panelOrder)>0) {
    for (i in 1:length(rv$panelOrder)) {
      
      # start off assuming that all panels are not modifed from default values
      rv$modified[rv$panelOrder[i]] = FALSE
      
      # obtain value of continuousMin input
      evalString <- paste('input$continuousMin',rv$panelOrder[i],sep='')
      val <- eval(parse(text=evalString))
      # if val exists (not true initially)
      if (!is.null(val)) {
        # if val is viable numeric string
        if (suppressWarnings(!is.na(as.numeric(val)))) {
          rv$continuousMin[[rv$panelOrder[i]]] = as.numeric(val)
          # on value changed from default
          if (as.numeric(val)!=-Inf) {
            rv$modified[rv$panelOrder[i]] = TRUE
          }
        }
      }
      
      # obtain value of continuousMax input
      evalString <- paste('input$continuousMax',rv$panelOrder[i],sep='')
      val <- eval(parse(text=evalString))
      # if val exists (not true initially)
      if (!is.null(val)) {
        # if val is viable numeric string
        if (suppressWarnings(!is.na(as.numeric(val)))) {
          rv$continuousMax[[rv$panelOrder[i]]] = as.numeric(val)
          # on value changed
          if (as.numeric(val)!=Inf) {
            rv$modified[rv$panelOrder[i]] = TRUE
          }
        }
      }
      
      # obtain value of missingValues input
      evalString <- paste('input$missingValues',rv$panelOrder[i],sep='')
      val <- eval(parse(text=evalString))
      
      rv$missingValues[[rv$panelOrder[i]]] = val
      # on value changed
      if (!is.null(rv$missingValues[[rv$panelOrder[i]]])) {
        rv$modified[rv$panelOrder[i]] = TRUE
      }
      
      # obtain value of integer selection
      evalString <- paste('input$varSelectInteger',rv$panelOrder[i],sep='')
      val <- eval(parse(text=evalString))
      rv$integerSelected[[rv$panelOrder[i]]] = val
      # on value changed
      if (!is.null(val)) {
        rv$modified[rv$panelOrder[i]] = TRUE
      }
      
      
    }
  }
}  # end of getWidgetValues


# perform subsetting
updateData <- function() {
  
  # remove columns
  sub = subset(mainData,select=setdiff(names(mainData),input$selectColumns))
  
  # subset based on all filter values
  filter = rep(TRUE,nrow(mainData))
  if (length(rv$panelOrder)>0) {
    for (i in 1:length(rv$panelOrder)) {
      thisCol = mainData[,rv$panelOrder[i]]
      
      # remove missing values
      if (length(rv$missingValues[[rv$panelOrder[i]]])>0) {
        for (j in 1:length(rv$missingValues[[rv$panelOrder[i]]])) {
          filter = filter * (thisCol!=rv$missingValues[[rv$panelOrder[i]]][j])
        }
      }
      
      # if continuous
      if (rv$class[rv$panelOrder[i]]=='numeric') {
        # filter based on min and max
        filter = filter * (thisCol>=rv$continuousMin[rv$panelOrder[i]])
        filter = filter * (thisCol<=rv$continuousMax[rv$panelOrder[i]])
        
      # if integer
      } else if (rv$class[rv$panelOrder[i]]=='integer') {
        filter = filter * (!thisCol%in%rv$integerSelected[[rv$panelOrder[i]]])
        
      # if factor
      } else if (rv$class[rv$panelOrder[i]]=='factor') {
        
      }
      
    }
  }
  NAs = which(is.na(filter))
  filter[NAs] = 0
  rv$subData = sub[filter==TRUE,,drop=FALSE]
  
}  # end of updateData

# ----------------------------------------------------------------------------------------------------

# 'update' and 'save' data buttons
output$updateButton <- renderUI({
  fluidRow(
    column(12,
           div(style="display:inline-block",
               tags$button(id='updateDataButton', type="button", class="btn action-button btn-primary", style='font-size:15px; text-align:center', HTML('<i class="icon-star"></i>Update Data')),
               tags$button(id='saveDataButton', type="button", class="btn action", style='font-size:15px; text-align:center', HTML('<i class="icon-star"></i>Save Data'))
           ),
           align='center')
  )
})

# on update button pressed
observe({
  updatePressed()
})
updatePressed <- eventReactive(input$updateDataButton, {
  getWidgetValues()
  updateData()
})


# selectize for choosing which columns are active
output$selectColumns <- renderUI({
  selectInput('selectColumns', label=NULL, choices=names(mainData), multiple=TRUE, selectize=FALSE, width='100%')
})


# dropdown menu for choosing filter variable
output$filterVariable <- renderUI({
  myChoices = setdiff(names(mainData),input$selectColumns)
  if (length(rv$panelOrder)>1) {
    myChoices = setdiff(myChoices,names(mainData)[rv$panelOrder[-1]])
  }
  selectizeInput('filterVariable',label=NULL,choices=c('',myChoices), multiple=FALSE, options=list(placeholder='select, search or input variable names', selectOnTab=TRUE, create=FALSE))
})

# on new variable selected
observe({
  filterVariableSelected()
})
filterVariableSelected <- eventReactive(input$filterVariable, {
  if (input$filterVariable!='') {
    
    # deal with old selection
    if (length(rv$panelOrder)>0) {
      if (rv$modified[rv$panelOrder[1]]==FALSE) {
        rv$panelOrder = rv$panelOrder[-1]
      }
    }
    
    # deal with new selection
    i <- which(names(mainData)==input$filterVariable)
    if (!(i%in%rv$panelOrder)) {
      rv$panelOrder = c(i,rv$panelOrder)
    }
    
  }
})

# produce sequence of violin plots showing subsetting
observe({
  if (length(rv$panelOrder)>0) {
    for (i in 1:length(rv$panelOrder)) {
      
      if (rv$class[rv$panelOrder[i]]=='numeric') {
        local({
          
          my_i = rv$panelOrder[i]
          continuousMin = rv$continuousMin[rv$panelOrder[i]]
          continuousMax = rv$continuousMax[rv$panelOrder[i]]
          plotName = paste('violinSubPlot',rv$panelOrder[i],sep='')
          
          # add to list of outputs
          output[[plotName]] <- renderPlot({
              bobViolinPlot(mainData[,my_i],continuousMin,continuousMax)
          },bg='transparent')
        })
      }
    }
  }
})


# produce sequence of bar plots showing subsetting
observe({
  if (length(rv$panelOrder)>0) {
    for (i in 1:length(rv$panelOrder)) {
      
      if (rv$class[rv$panelOrder[i]]=='integer') {
        local({
          
          my_i = rv$panelOrder[i]
          tab = table(mainData[,my_i])
          selected = setdiff(unique(mainData[,my_i]),as.numeric(rv$integerSelected[[my_i]]))
          selected = setdiff(selected,rv$missingValues[[my_i]])
          total = length(unique(mainData[,my_i]))
          plotName = paste('barSubPlot',my_i,sep='')
          
          # add to list of outputs
          output[[plotName]] <- renderPlot({
            bobBarSubplot(tab,selected,total)
          },bg='transparent')
        })
      }
    }
  }
})


# create current active subset panel
output$subsetPanels_current <- renderUI({
  if (length(rv$panelOrder)>0) {
    if (rv$selectColumnOn[rv$panelOrder[1]]) {
      
      # continuous (numeric) data
      if (rv$class[rv$panelOrder[1]]=='numeric') {
        panel = wellPanel(
          bobResetButton("varReset"),
          h5(names(mainData)[rv$panelOrder[1]]),
          bobText1(inputId=paste("continuousMin",rv$panelOrder[1],sep=''), label='subset from', value=rv$continuousMin[rv$panelOrder[1]], size='5px', placeholder='min'),
          bobText1(inputId=paste("continuousMax",rv$panelOrder[1],sep=''), label='to', value=rv$continuousMax[rv$panelOrder[1]], size='5px', placeholder='max'),
          selectizeInput(paste('missingValues',rv$panelOrder[1],sep=''), label='remove missing values', choices=unlist(rv$missingValues[rv$panelOrder[1]]), selected=unlist(rv$missingValues[rv$panelOrder[1]]), multiple=TRUE, width='100%',options=list(create=TRUE, placeholder='type missing values and press enter')),
          plotOutput(paste('violinSubPlot',rv$panelOrder[1],sep=''), height=30)
          ,style='padding: 10px')
        
      # factor data
      } else if (rv$class[rv$panelOrder[1]]=='factor') {
        panel = wellPanel(
          bobResetButton("varReset"),
          h5(names(mainData)[rv$panelOrder[1]]),
          selectInput(paste('varSelectFactor', rv$panelOrder[1],sep=''), label=NULL, choices=levels(mainData[,rv$panelOrder[1]]), selected=levels(mainData[,rv$panelOrder[1]])[1], multiple=TRUE, selectize=FALSE, width='70%')
          ,style='padding: 10px')
        
      # integer data
      } else if (rv$class[rv$panelOrder[1]]=='integer') {
        panel = wellPanel(
          bobResetButton("varReset"),
          h5(names(mainData)[rv$panelOrder[1]]),
          selectInput(paste('varSelectInteger', rv$panelOrder[1],sep=''), label='remove levels', choices=unique(mainData[,rv$panelOrder[1]]), selected=rv$integerSelected[[rv$panelOrder[1]]], multiple=TRUE, selectize=FALSE, width='70%'),
          selectizeInput(paste('missingValues',rv$panelOrder[1],sep=''), label='remove missing values', choices=unlist(rv$missingValues[rv$panelOrder[1]]), selected=unlist(rv$missingValues[rv$panelOrder[1]]), multiple=TRUE, width='100%',options=list(create=TRUE, placeholder='type missing values and press enter')),
          plotOutput(paste('barSubPlot',rv$panelOrder[1],sep=''), height=50)
          ,style='padding: 10px')
      }
      
      panel
    }
  }
})  # end of output$subsetPanels_current


# create 'locked in' subset panels
output$subsetPanels_locked <- renderUI({
  if (length(rv$panelOrder)>1) {
    
    panelList = list()
    for (i in 2:length(rv$panelOrder)) {
      if (rv$selectColumnOn[rv$panelOrder[i]]) {
        
        # continuous (numeric) data
        if (rv$class[rv$panelOrder[i]]=='numeric') {
          panelList[[i]] = wellPanel(
            bobCloseButton(paste("varClose",rv$panelOrder[i],sep='')),
            h5(names(mainData)[rv$panelOrder[i]]),
            bobText1(inputId=paste("continuousMin",rv$panelOrder[i],sep=''), label='subset from', value=rv$continuousMin[rv$panelOrder[i]], size='5px', placeholder='min'),
            bobText1(inputId=paste("continuousMax",rv$panelOrder[i],sep=''), label='to', value=rv$continuousMax[rv$panelOrder[i]], size='5px', placeholder='max'),
            selectizeInput(paste('missingValues',rv$panelOrder[i],sep=''), label='remove missing values', choices=unlist(rv$missingValues[rv$panelOrder[i]]), selected=unlist(rv$missingValues[rv$panelOrder[i]]), multiple=TRUE, width='100%',options=list(create=TRUE, placeholder='type missing values and press enter')),
            plotOutput(paste('violinSubPlot',rv$panelOrder[i],sep=''), height=30)
          ,style='padding: 10px')
        
        # factor data
        } else if (rv$class[rv$panelOrder[i]]=='factor') {
          panelList[[i]] = wellPanel(
            bobCloseButton(paste("varClose",rv$panelOrder[i],sep='')),
            h5(names(mainData)[rv$panelOrder[i]]),
            selectInput(paste(inputId='varSelectFactor', rv$panelOrder[i],sep=''), label=NULL, choices=levels(mainData[,rv$panelOrder[i]]), multiple=TRUE, selectize=FALSE, width='70%')
            ,style='padding: 10px')
        
        # integer data
        } else if (rv$class[rv$panelOrder[i]]=='integer') {
          panelList[[i]] = wellPanel(
            bobCloseButton(paste("varClose",rv$panelOrder[i],sep='')),
            h5(names(mainData)[rv$panelOrder[i]]),
            selectInput(paste('varSelectInteger', rv$panelOrder[i],sep=''), label='remove levels', choices=unique(mainData[,rv$panelOrder[i]]), selected=rv$integerSelected[[rv$panelOrder[i]]], multiple=TRUE, selectize=FALSE, width='70%'),
            selectizeInput(paste('missingValues',rv$panelOrder[i],sep=''), label='remove missing values', choices=unlist(rv$missingValues[rv$panelOrder[i]]), selected=unlist(rv$missingValues[rv$panelOrder[i]]), multiple=TRUE, width='100%',options=list(create=TRUE, placeholder='type missing values and press enter')),
            plotOutput(paste('barSubPlot',rv$panelOrder[i],sep=''), height=50)
            ,style='padding: 10px')
        }
        
      }
    }
    panelListOut = c(1,panelList)
    panelListOut[[1]] = h4('current active filters')
    panelListOut
  }
})  # end of output$subsetPanels_locked


# on reset button pressed
observe({
  resetPressed()
})
resetPressed <- eventReactive(input$varReset, {
  resetData(1)
})
resetData <- function(i) {
  rv$continuousMin[rv$panelOrder[i]] = -Inf
  rv$continuousMax[rv$panelOrder[i]] = Inf
  rv$missingValues[rv$panelOrder[i]] = NULL
  rv$modified[rv$panelOrder[i]] = FALSE
  rv$integerSelected[[rv$panelOrder[i]]] = NULL
  rv$factorSelected[[rv$panelOrder[i]]] = NULL
  updateData()
  
}


# on close buttons pressed
observe({
  # search over all panels
  if (length(rv$panelOrder)>0) {
    for (i in 1:length(rv$panelOrder)) {
      
      # obtain value of action button
      evalString <- paste('input$varClose',rv$panelOrder[i],sep='')
      buttonValue <- eval(parse(text=evalString))[1]
      
      # if buttonValue exists (not true initially)
      if (!is.null(buttonValue)) {
        
        # rv$closeButtonPressed[rv$panelOrder[i]] cannot be larger than buttonValue
        if (rv$closeButtonPressed[rv$panelOrder[i]]>buttonValue) {
          rv$closeButtonPressed[rv$panelOrder[i]] = 0
        }
      }
    }
  }
})
observe({
  # search over all panels
  if (length(rv$panelOrder)>0) {
    for (i in 1:length(rv$panelOrder)) {
      
      # obtain value of action button
      evalString <- paste('input$varClose',rv$panelOrder[i],sep='')
      buttonValue <- eval(parse(text=evalString))[1]
      
      # if buttonValue exists (not true initially)
       if (!is.null(buttonValue)) {
         
         # on button pressed
        if (buttonValue==(rv$closeButtonPressed[rv$panelOrder[i]]+1)) {
          rv$closeButtonPressed[rv$panelOrder[i]] = buttonValue
          resetData(i)
          rv$panelOrder = rv$panelOrder[rv$panelOrder!=rv$panelOrder[i]]
        }
       }
    }
  }
})


# render data table
output$mainDataTable <- renderDataTable({
  rv$subData
},options=list(scrollX='300px', scrollY='400px', searching=FALSE))


