
################################################################
## create single list of reactive values to handle subsetting ##
################################################################
# rv <- reactiveValues(
#   
#   selectColumnOn = rep(TRUE,ncol(mainData)),
#   panelOrder = NULL,
#   
#   class = mapply(class,mainData),
#   closeButtonPressed = rep(0,ncol(mainData)),
#   resetButtonPressed = rep(0, ncol(mainData)), 
#   continuousMin = rep(-Inf,ncol(mainData)),
#   continuousMax = rep(Inf,ncol(mainData)),
#   missingValues = replicate(ncol(mainData),NULL),
#   integerSelected = replicate(ncol(mainData),NULL),
#   integerMin = rep(-Inf,ncol(mainData)),
#   integerMax = rep(Inf,ncol(mainData)),
#   factorSelected = replicate(ncol(mainData),NULL),
#   modified = rep(FALSE,ncol(mainData)),
#   
#   subData = mainData
# )  # end of reactiveValues


## create empty rv list
rv <- reactiveValues()

## define values in observer
## to allow for data input changes
observe({
  ## read data
  mainData <- NULL
  mainData <- .get.data()
  
  if(!is.null(mainData)){
    rv$selectColumnOn <- rep(TRUE,ncol(mainData))
    rv$panelOrder <- NULL
    rv$class <- mapply(class,mainData)
    rv$closeButtonPressed <- rep(0,ncol(mainData))
    rv$resetButtonPressed <- rep(0, ncol(mainData))
    rv$continuousMin <- rep(-Inf,ncol(mainData))
    rv$continuousMax <- rep(Inf,ncol(mainData))
    rv$missingValues <- replicate(ncol(mainData),NULL)
    rv$integerSelected <- replicate(ncol(mainData),NULL)
    rv$integerMin <- rep(-Inf,ncol(mainData))
    rv$integerMax <- rep(Inf,ncol(mainData))
    rv$factorSelected <- replicate(ncol(mainData),NULL)
    rv$modified <- rep(FALSE,ncol(mainData))
      
    rv$subData <- mainData      
  }
}) # end of reactiveValues


##################################################
## (simple scratch pad for messing around with) ##
##################################################
output$scratchPad <- renderUI({
  wellPanel(
    h4('(scratch pad)'),
    p(rv$closeButtonPressed)
  )
})

# ----------------------------------------------------------------------------------------------------

####################################################
## set reactive values based on values in widgets ##
####################################################
getWidgetValues <- function() {
  
  ## read in data
  mainData <- NULL
  mainData <- .get.data()
  
  prev.factorSelected <- NULL
  
  ## search over all panels
  if (length(rv$panelOrder)>0) {
    
    for (i in 1:length(rv$panelOrder)) {
      
      ## define column selected
      col.num <- rv$panelOrder[i]
      ## get data for column col.num
      x <- mainData[,col.num]
      
      ## start off assuming that all panels are not modifed from default values
      rv$modified[col.num] <- FALSE
      
      ###############################
      ## continuous (numeric) data ##
      ###############################
      
      #       ## obtain value of continuousMin input
      #       evalString <- paste('input$continuousMin',rv$panelOrder[i],sep='')
      #       val <- eval(parse(text=evalString))
      #       ## if val exists (not true initially)
      #       if (!is.null(val)) {
      #         ## if val is viable numeric string
      #         #if (suppressWarnings(!is.na(as.numeric(val)))) {
      #         if(is.numeric(val)){  
      #           rv$continuousMin[[rv$panelOrder[i]]] = as.numeric(val)
      #           ## on value changed from default
      #           if (as.numeric(val)!=-Inf) {
      #             rv$modified[rv$panelOrder[i]] = TRUE
      #           }
      #         }
      #       }
      #       
      #       # obtain value of continuousMax input
      #       evalString <- paste('input$continuousMax',rv$panelOrder[i],sep='')
      #       val <- eval(parse(text=evalString))
      #       # if val exists (not true initially)
      #       if (!is.null(val)) {
      #         # if val is viable numeric string
      #         if (suppressWarnings(!is.na(as.numeric(val)))) {
      #           rv$continuousMax[[rv$panelOrder[i]]] = as.numeric(val)
      #           # on value changed
      #           if (as.numeric(val)!=Inf) {
      #             rv$modified[rv$panelOrder[i]] = TRUE
      #           }
      #         }
      #       }
      
      ## obtain continuousMin and continuousMax inputs from SLIDER:
      evalString <- paste("input$continuousValue", col.num, sep="")
      val <- eval(parse(text=evalString))
      
      ## if val exists (not true initially):
      if(!is.null(val)){
        ## AND if val is a viable numeric string:
        #if (suppressWarnings(!is.na(as.numeric(val)))) {
        if(is.numeric(val)){ 
          rv$continuousMin[[col.num]] <- as.numeric(val)[1]
          rv$continuousMax[[col.num]] <- as.numeric(val)[2]
          ## on value change from default:
          ## for min...
          if(as.numeric(val)[1] != min(x[!is.na(x)])){
            rv$modified[col.num] <- TRUE
          }
          ## and max...
          if(as.numeric(val)[2] != max(x[!is.na(x)])){
            rv$modified[col.num] <- TRUE
          }          
        }
      } # end continuous (numeric) data   
      
      #################
      ## factor data ##
      #################
      
      ## obtain factor levels selected
      evalString <- paste("input$varSelectFactor", col.num, sep="")
      val <- eval(parse(text=evalString))      
      if(!is.null(val)){ 
        ## in case previously updated, get previously selected levels
        if(!is.null(rv$factorSelected[[col.num]])){
          prev.factorSelected <- rv$factorSelected[[col.num]]
        }else{
          prev.factorSelected <- val
        }                
        if(!identical(prev.factorSelected, val)){
          rv$factorSelected[[col.num]] <- unique(c(prev.factorSelected, val))
        }else{
          rv$factorSelected[[col.num]] <- val
        }        
        ## on value change
        rv$modified[[col.num]] <- TRUE
      } # end factor data      
      
      
      ##################
      ## integer data ##
      ##################
      
      #       # obtain value of integer selection
      #       evalString <- paste('input$varSelectInteger',rv$panelOrder[i],sep='')
      #       val <- eval(parse(text=evalString))
      #       rv$integerSelected[[rv$panelOrder[i]]] = val
      #       # on value changed
      #       if (!is.null(val)) {
      #         rv$modified[rv$panelOrder[i]] = TRUE
      #       }
            
      ## obtain values of integer range selection from slider
      evalString <- paste('input$integerValue',col.num,sep='')
      val.slider <- eval(parse(text=evalString)) 
      if(!is.null(val.slider)){
        rv$integerMin[[col.num]] <- as.integer(val.slider)[1]
        rv$integerMax[[col.num]] <- as.integer(val.slider)[2]
        if(as.integer(val.slider)[1] != min(x[!is.na(x)])) rv$modified[[col.num]] <- TRUE
        if(as.integer(val.slider)[2] != max(x[!is.na(x)])) rv$modified[[col.num]] <- TRUE
      }      
      
      ## obtain value of (individual) integer selection from selectize
      evalString <- paste('input$varSelectInteger',col.num,sep='')
      val.selectize <- eval(parse(text=evalString))     
      if(!is.null(val.selectize)){
        ## in case previously updated, get previously selected integers
        if(!is.null(rv$integerSelected[[col.num]])){
          prev.integerSelected <- rv$integerSelected[[col.num]]
        }else{
          prev.integerSelected <- val.selectize
        }
        if(!identical(prev.integerSelected, val.selectize)){
          rv$integerSelected[[col.num]] <- unique(c(prev.integerSelected, val.selectize))
        }else{
          rv$integerSelected[[col.num]] <- val.selectize
        } 
        ## on value change
        rv$modified[[col.num]] <- TRUE
      } # end integer data

      
      
      ####################      
      ## missing values ##
      ####################
      # obtain value of missingValues input
      evalString <- paste('input$missingValues',col.num,sep='')
      val <- eval(parse(text=evalString))
      
      rv$missingValues[[col.num]] = val
      # on value changed
      if (!is.null(rv$missingValues[[col.num]])) {
        rv$modified[col.num] = TRUE
      } # end missing values
      
      
    } # end for loop

  }
}  # end of getWidgetValues




########################
## perform subsetting ##
########################
updateData <- function() {    
  
  ## read in data
  mainData <- NULL
  mainData <- .get.data()
  
  # remove columns
  sub = subset(mainData, 
               select=setdiff(names(mainData), 
                              input$selectColumns))
  
  # subset based on all filter values
  filter = rep(TRUE, nrow(mainData))
  if (length(rv$panelOrder)>0) {  
    
    for (i in 1:length(rv$panelOrder)) {
      
      ## define column selected
      col.num <- rv$panelOrder[i]
      ## get data for column col.num
      x <- mainData[,col.num]       
      
      
      ## remove missing values
      if (length(rv$missingValues[[col.num]])>0) {
        for (j in 1:length(rv$missingValues[[col.num]])) {
          filter = filter * (x!=rv$missingValues[[col.num]][j])
        }
      }
      
      ## if continuous
      if (rv$class[col.num]=='numeric') {
        ## filter based on min and max
        
        #filter = filter * (thisCol>=rv$continuousMin[rv$panelOrder[i]])
        #filter = filter * (thisCol<=rv$continuousMax[rv$panelOrder[i]])
        
        filter = filter * (x >= rv$continuousMin[[col.num]])
        filter = filter * (x <= rv$continuousMax[[col.num]])
        
        ## if integer
      } else if (rv$class[col.num]=='integer') {
        filter = filter * (!x%in%rv$integerSelected[[col.num]])
        filter = filter * (x >= rv$integerMin[[col.num]])
        filter = filter * (x <= rv$integerMax[[col.num]])
        
        ## if factor
      } else if (rv$class[col.num]=='factor') {
        filter = filter * (!x%in%rv$factorSelected[[col.num]])
      }      
      
    } # end for loop
  }
  
  NAs = which(is.na(filter))
  filter[NAs] = 0    
  rv$subData = sub[filter==TRUE,,drop=FALSE]  
  
}  # end of updateData

# ----------------------------------------------------------------------------------------------------


######################################
## 'update' and 'save' data buttons ##
######################################
output$updateButton <- renderUI({
  fluidRow(
    column(12,
           div(style="display:inline-block",
               tags$button(id='updateDataButton', type="button", 
                           class="btn action-button btn-primary", 
                           style='font-size:15px; text-align:center', 
                           HTML('<i class="icon-star"></i>Update Data')),
               
               tags$button(id='saveDataButton', type="button", 
                           class="btn action", 
                           style='font-size:15px; text-align:center', 
                           HTML('<i class="icon-star"></i>Save Data'))
           ),
           align='center')
  )
})



##############################
## on update button pressed ##
##############################
observe({
  updatePressed()
})
updatePressed <- eventReactive(input$updateDataButton, {
  getWidgetValues()
  updateData()
})




#####################################################
## selectize for choosing which columns are active ##
#####################################################
output$selectColumns <- renderUI({  
  
  ## read in data
  mainData <- NULL
  mainData <- .get.data()
  
  selectInput('selectColumns', label=NULL, 
              choices=names(mainData), multiple=TRUE, 
              selectize=FALSE, width='100%')
})


################################################
## dropdown menu for choosing filter variable ##
################################################
output$filterVariable <- renderUI({
  
  ## read in data
  mainData <- NULL
  mainData <- .get.data()
  
  myChoices = setdiff(names(mainData),input$selectColumns)
  if (length(rv$panelOrder)>1) {
    myChoices = setdiff(myChoices,names(mainData)[rv$panelOrder[-1]])
  }
  selectizeInput('filterVariable',label=NULL,
                 choices=c('',myChoices), multiple=FALSE, 
                 options=list(placeholder='select, search or input variable names', 
                              selectOnTab=TRUE, create=FALSE))
})


##############################
## on new variable selected ##
##############################
observe({
  filterVariableSelected()
})

filterVariableSelected <- eventReactive(input$filterVariable, {
  
  ## read in data
  mainData <- NULL
  mainData <- .get.data()
  
  if (input$filterVariable!="") {
    
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



#########################################################
## produce sequence of violin plots showing subsetting ##
#########################################################
# observe({
#   if (length(rv$panelOrder)>0) {
#     for (i in 1:length(rv$panelOrder)) {
#       
#       if (rv$class[rv$panelOrder[i]]=='numeric') {
#         local({
#           
#           my_i = rv$panelOrder[i]
#           continuousMin = rv$continuousMin[rv$panelOrder[i]]
#           continuousMax = rv$continuousMax[rv$panelOrder[i]]
#           plotName = paste('violinSubPlot',rv$panelOrder[i],sep='')
#           
#           # add to list of outputs
#           output[[plotName]] <- renderPlot({
#             bobViolinPlot(mainData[,my_i],continuousMin,continuousMax)
#           },bg='transparent')
#         })
#       }
#     }
#   }
# })


######################################################
## produce sequence of bar plots showing subsetting ##
######################################################
# observe({
#   if (length(rv$panelOrder)>0) {
#     for (i in 1:length(rv$panelOrder)) {
#       
#       ## get column selected
#       col.num <- rv$panelOrder[i]
#       ## get data for column col.num
#       x <- mainData[,col.num]
#       
#       ## for CURRENT panel, enable automatic plot updating:
#       if(i == 1){
#         
#         ## get selectize input ##
#         evalString <- paste("input$varSelectInteger", col.num, sep="")
#         selector <- eval(parse(text=evalString))
#         
#         ## get slider input ##
#         evalString <- paste("input$integerValue", col.num, sep="")
#         slider <- eval(parse(text=evalString))        
#         
#         if(!is.null(slider)){
#           local({
#             
#             #my_i = rv$panelOrder[i]
#             tab = table(x)            
#             
#             ## get set of integers
#             selected <- unique(x)
#             
#             ## handle SLIDER ##
#             #selected = setdiff(selected, selected[selected < rv$integerMin[[col.num]]])
#             #selected = setdiff(selected, selected[selected > rv$integerMax[[col.num]]])
#             selected = setdiff(selected, selected[selected < slider[1]])
#             selected = setdiff(selected, selected[selected > slider[2]])      
#             
#             ## handle SELECTIZE ##
#             #selected = setdiff(unique(x), as.numeric(rv$integerSelected[[col.num]]))
#             selected = setdiff(selected, as.integer(selector))
#             
#             ## handle MISSING ##
#             #selected = setdiff(selected,rv$missingValues[[col.num]])
#             #selected = setdiff(selected,rv$missingValues[[col.num]])
#             
#             total = length(unique(x))
#             plotName = paste('barSubPlot', col.num, sep="")
#             
#             # add to list of outputs
#             output[[plotName]] <- renderPlot({
#               bobBarSubplot(tab, selected, total)
#             },bg='transparent')
#           })
#         }
#         
#       }else{
#       ## for previous panels, get settings for plot from rv:
#       if (rv$class[col.num]=='integer') {
#         local({
#           
#           #my_i = rv$panelOrder[i]
#           tab = table(x)
#           selected = setdiff(unique(x), as.numeric(rv$integerSelected[[col.num]]))
#           selected = setdiff(selected, selected[selected < rv$integerMin[[col.num]]])
#           selected = setdiff(selected, selected[selected > rv$integerMax[[col.num]]])
#           selected = setdiff(selected,rv$missingValues[[col.num]])
#           total = length(unique(x))
#           plotName = paste('barSubPlot', col.num, sep="")
#           
#           # add to list of outputs
#           output[[plotName]] <- renderPlot({
#             bobBarSubplot(tab,selected,total)
#           },bg='transparent')
#         })
#       }
#       }
#     }
#   }
# }) # end barplot observer















######################
## .getSubsetWidget ##
######################
.getSubsetWidget <- function(rv, i, input){
  
  ## read in data
  mainData <- NULL
  mainData <- .get.data()
  
  out <- NULL                
  
  ## define column selected
  col.num <- rv$panelOrder[i]
  ## get data for that column
  x <- mainData[,col.num]
  
  ## get values for missing values selectize
  missingId <- paste("missingValues", col.num, sep="")
  missingLabel <- "Remove the following ''missing'' values:"
  missingChoices <- unlist(rv$missingValues[col.num]) # c(NA, "N/A", "-", ".", "", 0)
  missingSelected <- unlist(rv$missingValues[col.num])     
  
  
  ################
  ## .getButton ##
  ################    
  ## Generates BOTH reset AND close buttons for ALL panels (present and previous)  
  ## NOTE: WAS THERE A REASON WE DIDN'T ALLOW USERS TO RESET PREVIOUS PANELS OR WAS THIS JUST NOT IMPLEMENTED ????
  .getButton <- function(i){
    out <- NULL
    
    col.num <- rv$panelOrder[i]
    
    out <- list()
    out[[1]] <- bobCloseButton(paste("varClose", col.num, sep=""))
    out[[2]] <- bobResetButton(paste("varReset", col.num, sep=""))
    
    return(out)
  } # end .getButton
  
  
  #################
  ## .getBarPlot ##
  #################
  .getBarPlot <- function(col.num, varClass="factor", input){
    
    out <- NULL
    
    ## get data for column  col.num
    x <- mainData[,col.num]                          
    
    if(varClass=="factor"){                      
      
      ## FACTORS ##
      
      ## get selectize input
      evalString <- paste("input$varSelectFactor", col.num, sep="")
      selector <- eval(parse(text=evalString))
      
      ## get slider input
      #evalString <- paste("input$integerValue", col.num, sep="")
      #slider <- eval(parse(text=evalString))
      slider <- NULL
      
    }else{
      
      ## INTEGERS ##
      
      ## get selectize input
      evalString <- paste("input$varSelectInteger", col.num, sep="")
      selector <- eval(parse(text=evalString))
      ## get slider input
      evalString <- paste("input$integerValue", col.num, sep="")
      slider <- eval(parse(text=evalString))
    }
    
    tab <- table(x)            
    
    ## get set of integers
    selected <- unique(x)
    
    ## handle previously removed values
    subData <- rv$subData
    sub.col.num <- which(names(subData) == names(mainData)[col.num])
    prev.removed <- selected[which(!selected %in% unique(subData[,sub.col.num]))]      
    
    ## handle SLIDER ##
    if(!is.null(slider)){
      selected = setdiff(selected, selected[selected < slider[1]])
      selected = setdiff(selected, selected[selected > slider[2]])      
    }
    ## handle SELECTIZE ##
    selected = setdiff(selected, selector)
    
    ## handle MISSING ##
    selected = setdiff(selected, rv$missingValues[[col.num]])
        
    ## handle PREVIOUS
    selected <- setdiff(selected, prev.removed)
    
    total <- length(unique(x))    
    
    out <- bobBarSubplot(tab, selected, total, varClass=varClass)
    
    return(out)    
    
  } # end .getBarPlot
  
  

  ############################################################################################################################  
  
    ###############################
    ## continuous (numeric) data ##
    ###############################
    if (rv$class[col.num]=='numeric') {          
      
      ## get values for slider
      sliderId <- paste("continuousValue", col.num ,sep="")
      sliderLabel <- "Select subset range:" 
      minSelected <- continuousMin <- min(x[!is.na(x)])
      maxSelected <- continuousMax <- max(x[!is.na(x)])
      ## update continuousMin if it has changed from min
      if(rv$continuousMin[[col.num]] != -Inf){
        if(rv$continuousMin[[col.num]] != min(x[!is.na(x)])){
          minSelected <- rv$continuousMin[[col.num]]
        }
      }
      ## update continuousMax if it has changed from max
      if(rv$continuousMax[[col.num]] != Inf){
        if(rv$continuousMax[[col.num]] != max(x[!is.na(x)])){
          maxSelected <- rv$continuousMax[[col.num]]
        }
      }
      
      #plotName <- paste("violinSubPlot", col.num, sep="")
      
      ####################
      ## .fetchMyViolin ##
      ####################
      .fetchMyViolin <- function(col.num, input){
        out <- NULL
        x <- mainData[,col.num]
        evalString <- paste("input$continuousValue", col.num, sep="")
        slider <- eval(parse(text=evalString))  
        minSelected <- slider[1]
        maxSelected <- slider[2]
        #if(minSelected != -Inf || maxSelected != Inf){
          out <- bobViolinPlot(x, minSelected, maxSelected)
        #}
        return(out)
      } # end .fetchMyViolin
      
      
      
      ## define output ##
      out <- list(
        
        ## define panel ##            
        wellPanel(
          
          ## RESET or CLOSE button
          #bobResetButton("varReset"),
          .getButton(i),
          
          ## header
          span(h4(strong(names(mainData)[col.num])) , 
               style = "color: rgb(0,111,188)"), # rgb(0, 108, 196)
          
          ## slider
          sliderInput(inputId = sliderId, 
                      label = sliderLabel,
                      min = continuousMin,
                      max = continuousMax,
                      value = c(minSelected, maxSelected),
                      step=NULL
          ),
          
          ## missing values
          selectizeInput(inputId = missingId, 
                         label = missingLabel, 
                         choices = missingChoices, 
                         selected = missingSelected, 
                         multiple=TRUE, 
                         width='100%', 
                         options=list(create=TRUE, 
                                      placeholder="Select or define* 'missing values' to remove")),
          
          ## violin plot
          #plotOutput((plotName), height=30), 
          renderPlot({.fetchMyViolin(col.num, input)}, height=30),
          
          style='padding: 10px'),
        
        
        ## add helpText
        helpText("*If", em("defining"), "values, hit ENTER between each")
        
      ) # end list
      
      # end continuous (numeric) data
      
      ############################################################################################################################
      
      #################
      ## factor data ##
      #################
    }else if(rv$class[col.num] == "factor"){
      
      ## get values for factor level selectize/selectInput
      factorId <- paste("varSelectFactor", col.num, sep="")
      factorLabel <- "Select levels to remove:"
      factorChoices <- levels(x)
      factorSelected <- NULL # rv$factorSelected[[col.num]] # levels(x)[1]
      
      ## update factorSelected if it has changed from NULL
      if(!is.null(rv$factorSelected[[col.num]])){
        factorSelected <- as.character(rv$factorSelected[[col.num]])
        ## and update choices to reflect only those REMAINING (only for selectInput)
        if(length(levels(x)) > 1000) factorChoices <- factorChoices[-which(factorChoices %in% factorSelected)]
      }
      
      ########################
      ## .factorSelector fn ##
      ########################
      .factorSelector <- function(x){
        out <- NULL
        if(length(levels(x)) <= 1000){
          
          # factor level selectize** ##
          # (**ONLY WORKS IF VARIABLE HAS <= 1000 LEVELS)
          out <- selectizeInput(inputId = factorId,
                                label = factorLabel,
                                choices = factorChoices,
                                selected = factorSelected,
                                multiple=TRUE, 
                                options=list(placeholder="select or search for levels", 
                                             delimiter=",", 
                                             allowEmptyOptions=TRUE, 
                                             selectOnTab=TRUE, 
                                             create=TRUE),
                                width='70%')   
        }else{
          ## factor level selectInput ##
          out <- selectInput(inputId = factorId,
                             label = factorLabel,
                             choices = factorChoices,
                             selected = factorSelected,
                             multiple=TRUE, 
                             selectize = FALSE,
                             width='70%') 
        }     
        return(out)
      } # end .factorSelector
      

      
      ## define panel ##
      out <- wellPanel(        
        
        ## RESET or CLOSE button
        #bobResetButton("varReset"),
        .getButton(i),
        
        ## header ##
        span(h4(strong(names(mainData)[col.num])) , 
             style = "color: rgb(0,111,188)"), # rgb(0, 108, 196)  
        
        ## factor level selectize/selectInput | length(levels(x))
        .factorSelector(x),
        
        ## missing values
        selectizeInput(inputId = missingId, 
                       label = missingLabel, 
                       choices = missingChoices, 
                       selected = missingSelected, 
                       multiple=TRUE, 
                       width='100%', 
                       options=list(create=TRUE, 
                                    placeholder="Select or define* 'missing values' to remove")),                
        
        ## bar plot (for ordered factors only??):
        renderPlot({.getBarPlot(col.num, varClass="factor", input)}, height=50),
                
        style='padding: 10px')    
            
      # end factor data
      
      ############################################################################################################################
      
      ##################
      ## integer data ##
      ##################
    }else if(rv$class[col.num] == "integer"){
      
      ## ENABLE BOTH SLIDER- AND "LEVEL"-BASED SUBSET SELECTION FOR INTEGERS ##
      
      
      ## get values for slider
      sliderId <- paste("integerValue", col.num ,sep="")
      sliderLabel <- "Select subset range:"             
      minSelected <- integerMin <- min(x[!is.na(x)])
      maxSelected <- integerMax <- max(x[!is.na(x)])
      
      ## update integerMin if it has changed from min
      if(rv$integerMin[[col.num]] != -Inf){
        if(rv$integerMin[[col.num]] != min(x[!is.na(x)])){
          minSelected <- rv$integerMin[[col.num]]
        }
      }
      ## update integerMax if it has changed from max
      if(rv$integerMax[[col.num]] != Inf){
        if(rv$integerMax[[col.num]] != max(x[!is.na(x)])){
          maxSelected <- rv$integerMax[[col.num]]
        }
      }
      
      
      ## get values for individual integer (~ factor level) selectize            
      ################## TO DO : SHOULD REMOVE INTEGERS OUTSIDE CURRENT SLIDER RANGE [integerMin, integerMax] !!!!            
      integerId <- paste("varSelectInteger", col.num, sep="")
      integerLabel <- "Select values to remove:"
      integerChoices <- unique(x)
      integerSelected <- rv$integerSelected[[col.num]] # NULL
      
      ## update integerSelected if it has changed from NULL
      if(!is.null(rv$integerSelected[[col.num]])){
        ## update selected
        integerSelected <- as.numeric(rv$integerSelected[[col.num]])
        ## update choices to reflect only those REMAINING (only IF using selectInput)              
        if(length(unique(x)) > 1000) integerChoices <- integerChoices[-which(integerChoices %in% integerSelected)]                             
      }
      
      
      #########################
      ## .integerSelector fn ##
      #########################            
      .integerSelector <- function(x){
        out <- NULL
        
        ## use selectize
        if(length(unique(x)) <= 1000){
          ## individual integer selectize ##
          out <- selectizeInput(inputId = integerId,
                                label = integerLabel,
                                choices = integerChoices,
                                selected = integerSelected,
                                multiple=TRUE, 
                                options=list(placeholder="select or search for integers", 
                                             delimiter=",", 
                                             allowEmptyOptions=TRUE, 
                                             selectOnTab=TRUE, 
                                             create=TRUE),
                                width='70%')
        }else{
          ## use selectInput
          out <- selectInput(inputId = integerId,
                             label = integerLabel,
                             choices = integerChoices,
                             selected = integerSelected,
                             multiple=TRUE, 
                             selectize = FALSE,
                             width='70%')
          
        }
        return(out)
      } # end .integerSelector
      

            
      
      
      ## define output ##
      out <- list(
        
        ## define panel ##            
        wellPanel(
          
          ## RESET or CLOSE button
          #bobResetButton("varReset"),
          .getButton(i),
          
          ## header
          span(h4(strong(names(mainData)[col.num])) , 
               style = "color: rgb(0,111,188)"), # rgb(0, 108, 196)
          
          ## slider
          sliderInput(inputId = sliderId, 
                      label = sliderLabel,
                      min = integerMin,
                      max = integerMax,
                      value = c(minSelected, maxSelected),
                      step=1),
          
          ## select individual integers by selectInput/selectize
          .integerSelector(x),
          
          ## missing values
          selectizeInput(inputId = missingId, 
                         label = missingLabel, 
                         choices = missingChoices, 
                         selected = missingSelected, 
                         multiple=TRUE, 
                         width='100%', 
                         options=list(create=TRUE, 
                                      placeholder="Select or define* 'missing values' to remove")),
          
          ## bar plot
          #plotOutput(paste('barSubPlot', col.num, sep=""), height=50),   
          renderPlot({.getBarPlot(col.num, varClass="integer", input)}, height=50),
          
          style='padding: 10px'),
        
        
        ## add helpText
        helpText("*If", em("defining"), "values, hit ENTER between each")
        
      ) # end list                          
      
      
    } # end integer data        
  
  return(out)
  
} # end .getSubsetWidget
















########################################
## create current active subset panel ##
########################################
output$subsetPanels_current <- renderUI({
  
  ## read in data
  mainData <- NULL
  mainData <- .get.data()  
  
  if (length(rv$panelOrder)>0) {
    if (rv$selectColumnOn[rv$panelOrder[1]]) {    
      
      ###############
      ## get panel ##
      ###############
      
      panel <- .getSubsetWidget(rv, 1, input)      
      
      ########################################
      
      
      
      #       ## continuous (numeric) data ##
      #       if (rv$class[rv$panelOrder[1]]=='numeric') {
      
      #        panel <- list(## call fn creating wellPanel w widgets
      #                       .getSubsetWidget(rv, 1, input), 
      #                       ## add help text
      #                       helpText("*If", em("defining"), "values, hit ENTER between each"))
      
      #         panel = wellPanel(
      #           bobResetButton("varReset"),
      #           
      #           #h5(names(mainData)[rv$panelOrder[1]]),          
      #           
      #           span(h4(strong(names(mainData)[rv$panelOrder[1]])) , 
      #                style = "color: rgb(0,108,196)"), 
      #           
      #           br(),
      #                     
      #           bobText1(inputId=paste("continuousMin",rv$panelOrder[1],sep=''), 
      #                    label='Subset from', 
      #                    value=rv$continuousMin[rv$panelOrder[1]], 
      #                    size='5px', 
      #                    placeholder='min'),
      #           
      #           bobText1(inputId=paste("continuousMax",rv$panelOrder[1],sep=''), 
      #                    label='to', 
      #                    value=rv$continuousMax[rv$panelOrder[1]], 
      #                    size='5px', 
      #                    placeholder='max'),
      #           
      #           
      #           selectizeInput(paste('missingValues',rv$panelOrder[1],sep=''), 
      #                          label='Remove missing values', 
      #                          choices=unlist(rv$missingValues[rv$panelOrder[1]]), 
      #                          selected=unlist(rv$missingValues[rv$panelOrder[1]]), 
      #                          multiple=TRUE, 
      #                          width='100%', 
      #                          options=list(create=TRUE, 
      #                                       placeholder='type missing values and press enter')),
      #           
      #           plotOutput(paste('violinSubPlot',rv$panelOrder[1],sep=''), height=30),
      #           
      #           style='padding: 10px')
      
      
      #       ## factor data ##
      #       } else if (rv$class[rv$panelOrder[1]]=='factor') {
      #         panel = wellPanel(
      #           bobResetButton("varReset"),
      #           h5(names(mainData)[rv$panelOrder[1]]),
      #           
      #           selectInput(paste('varSelectFactor', rv$panelOrder[1],sep=''), 
      #                       label=NULL, 
      #                       choices=levels(mainData[,rv$panelOrder[1]]), 
      #                       selected=levels(mainData[,rv$panelOrder[1]])[1], 
      #                       multiple=TRUE, selectize=FALSE, width='70%'),
      #           
      #           style='padding: 10px')
      
      
      #       ## integer data ##
      #       } else if (rv$class[rv$panelOrder[1]]=='integer') {
      #         panel = wellPanel(
      #           bobResetButton("varReset"),
      #           h5(names(mainData)[rv$panelOrder[1]]),
      #           
      #           selectInput(paste('varSelectInteger', rv$panelOrder[1],sep=''), 
      #                       label='remove levels', 
      #                       choices=unique(mainData[,rv$panelOrder[1]]), 
      #                       selected=rv$integerSelected[[rv$panelOrder[1]]], 
      #                       multiple=TRUE, selectize=FALSE, width='70%'),
      #           
      #           selectizeInput(paste('missingValues',rv$panelOrder[1],sep=''), 
      #                          label='remove missing values', 
      #                          choices=unlist(rv$missingValues[rv$panelOrder[1]]), 
      #                          selected=unlist(rv$missingValues[rv$panelOrder[1]]), 
      #                          multiple=TRUE, width='100%', 
      #                          options=list(create=TRUE, 
      #                                       placeholder='type missing values and press enter')),
      #           
      #           plotOutput(paste('barSubPlot',rv$panelOrder[1],sep=''), height=50),
      #           
      #           style='padding: 10px')
      #       }
      
      #panel
      panelListOut <- list()
      panelListOut[[1]] <- h4(strong("Current active filters:"))
      panelListOut[[2]] <- panel
      panelListOut
    }
  }
})  # end of output$subsetPanels_current



######################################
## create 'locked in' subset panels ##
######################################
output$subsetPanels_locked <- renderUI({
  
  ## read in data
  mainData <- NULL
  mainData <- .get.data()
  
  if (length(rv$panelOrder)>1) {
    
    panelList = list()
    for (i in 2:length(rv$panelOrder)) {      
      if (rv$selectColumnOn[rv$panelOrder[i]]) {
        
        #######################
        ## add panel to list ##
        #######################
        
        panelList[[i]] <- .getSubsetWidget(rv, i, input)
        
        #################################################
        
        #         ## continuous (numeric) data ##
        #         if (rv$class[rv$panelOrder[i]]=='numeric') {
        #           panelList[[i]] = wellPanel(
        #             bobCloseButton(paste("varClose",rv$panelOrder[i],sep='')),
        #             h5(names(mainData)[rv$panelOrder[i]]),
        #             
        #             bobText1(inputId=paste("continuousMin",rv$panelOrder[i],sep=''), 
        #                      label='Subset from:', 
        #                      value=rv$continuousMin[rv$panelOrder[i]], 
        #                      size='5px', 
        #                      placeholder='min'),
        #             
        #             bobText1(inputId=paste("continuousMax",rv$panelOrder[i],sep=''), 
        #                      label='to', 
        #                      value=rv$continuousMax[rv$panelOrder[i]], 
        #                      size='5px', 
        #                      placeholder='max'),
        #             
        #             selectizeInput(paste('missingValues',rv$panelOrder[i],sep=''), 
        #                            label='remove missing values', 
        #                            choices=unlist(rv$missingValues[rv$panelOrder[i]]), 
        #                            selected=unlist(rv$missingValues[rv$panelOrder[i]]), 
        #                            multiple=TRUE, 
        #                            width='100%', 
        #                            options=list(create=TRUE, 
        #                                         placeholder='type missing values and press enter')),
        #             
        #             plotOutput(paste('violinSubPlot',rv$panelOrder[i],sep=''), height=30),
        #             
        #             style='padding: 10px')
        #           
        #           
        #           ## factor data ##
        #         } else if (rv$class[rv$panelOrder[i]]=='factor') {
        #           panelList[[i]] = wellPanel(
        #             bobCloseButton(paste("varClose",rv$panelOrder[i],sep='')),
        #             h5(names(mainData)[rv$panelOrder[i]]),
        #             
        #             selectInput(paste(inputId='varSelectFactor', rv$panelOrder[i],sep=''), 
        #                         label=NULL, 
        #                         choices=levels(mainData[,rv$panelOrder[i]]), 
        #                         multiple=TRUE, 
        #                         selectize=FALSE, 
        #                         width='70%'),
        #             
        #             style='padding: 10px')
        #           
        #           
        #           ## integer data ##
        #         } else if (rv$class[rv$panelOrder[i]]=='integer') {
        #           panelList[[i]] = wellPanel(
        #             bobCloseButton(paste("varClose",rv$panelOrder[i],sep='')),
        #             h5(names(mainData)[rv$panelOrder[i]]),
        #             
        #             selectInput(paste('varSelectInteger', rv$panelOrder[i],sep=''), 
        #                         label='remove levels', 
        #                         choices=unique(mainData[,rv$panelOrder[i]]), 
        #                         selected=rv$integerSelected[[rv$panelOrder[i]]], 
        #                         multiple=TRUE, 
        #                         selectize=FALSE, 
        #                         width='70%'),
        #             
        #             selectizeInput(paste('missingValues',rv$panelOrder[i],sep=''), 
        #                            label='remove missing values', 
        #                            choices=unlist(rv$missingValues[rv$panelOrder[i]]), 
        #                            selected=unlist(rv$missingValues[rv$panelOrder[i]]), 
        #                            multiple=TRUE, 
        #                            width='100%', 
        #                            options=list(create=TRUE, 
        #                                         placeholder='type missing values and press enter')),
        #             
        #             plotOutput(paste('barSubPlot',rv$panelOrder[i],sep=''), height=50),
        #             
        #             style='padding: 10px')
        #         }
        #         
        
       
        
      }
    }
    #panelListOut = c(1, panelList)
    #panelListOut[[1]] = h4(strong("current active filters:"))
    #panelListOut 
    panelList
  }
})  # end of output$subsetPanels_locked




#############################
## on reset button pressed ##
#############################
observe({
  
  ## read in data
  mainData <- NULL
  mainData <- .get.data()
  
  if(length(rv$panelOrder) > 0){
    for(i in 1:length(rv$panelOrder)){
      col.num <- rv$panelOrder[i]
      resetValue <- NULL
      # obtain value of action button
      evalString <- paste("input$varReset", col.num, sep="")
      resetValue <- eval(parse(text=evalString))[1]
      
      #print("rv$panelOrder"); print(rv$panelOrder)      
      #print("i"); print(i)
      #print("resetValue"); print(resetValue)
      
      if(!is.null(resetValue)){
        if(resetValue != 0){
         
          ## define resetPressed
          resetPressed <- eventReactive({
            #eval(parse(text=paste("input$varReset", col.num, sep="")))[1]
            resetValue
          },{
            resetData(i=i, input, mainData)
            #i = which(rv$panelOrder == col.num)
          }
          ) # end resetPressed
          
          ## run resetPressed
          resetPressed()
        }
      }
    }
  }  
})

 
# resetPressed <- eventReactive(input$varReset, {  
#   resetData(i=1, input, mainData)
# })


###############
## resetData ##
###############
resetData <- function(i=1, input, mainData) {
  
  ## get column selected
  col.num <- rv$panelOrder[i]
  ## get data for column col.num
  x <- mainData[,col.num]
  
  if(rv$class[[col.num]] == "numeric"){
  #   rv$continuousMin[rv$panelOrder[i]] = -Inf
  #   rv$continuousMax[rv$panelOrder[i]] = Inf
  rv$continuousMin[col.num] = min(x[!is.na(x)])
  rv$continuousMax[col.num] = max(x[!is.na(x)])
  }
  
  rv$missingValues[col.num] = NULL # c(NA, "N/A", "-", ".", "", 0) # 
  rv$modified[col.num] = FALSE
  
  if(rv$class[[col.num]] == "integer"){
  rv$integerSelected[[col.num]] = NULL
  rv$integerMin[col.num] = min(x[!is.na(x)])
  rv$integerMax[col.num] = max(x[!is.na(x)])
  }
  
  rv$factorSelected[[col.num]] = NULL
  ## need to keep this as a record of button presses
  #rv$closeButtonPressed[[col.num]] <- 0
  
  updateData()
  
} # end resetData


##############################
## on close buttons pressed ##
##############################
# observe({
#   # search over all panels
#   if (length(rv$panelOrder)>0) {
#     for (i in 1:length(rv$panelOrder)) {
#       
#       ## define column selected
#       col.num <- rv$panelOrder[i]
#       
#       # obtain value of action button
#       evalString <- paste('input$varClose',col.num,sep='')
#       buttonValue <- eval(parse(text=evalString))[1]
#              
#       
#       # if buttonValue exists (not true initially)
#       if (!is.null(buttonValue)) {        
#         # rv$closeButtonPressed[rv$panelOrder[i]] cannot be larger than buttonValue
#         if (rv$closeButtonPressed[col.num] > buttonValue) {
#           rv$closeButtonPressed[col.num] = 0
#         }
#       }
#     }
#   }
# })

# observe({
#   # search over all panels
#   if (length(rv$panelOrder)>0) {
#     
#     for (i in 1:length(rv$panelOrder)) {
#     #for (i in 2:length(rv$panelOrder)) {        
#       
#       ## define column selected
#       col.num <- rv$panelOrder[i]
#       
#       # obtain value of action button
#       evalString <- paste('input$varClose',col.num,sep='')
#       buttonValue <- eval(parse(text=evalString))[1]
# 
#       # if buttonValue exists (not true initially)
#       if (!is.null(buttonValue)) {
#         
#         # rv$closeButtonPressed[rv$panelOrder[i]] cannot be larger than buttonValue
#         if (rv$closeButtonPressed[col.num] > buttonValue) {
#           rv$closeButtonPressed[col.num] = 0
#         }
#         
#         # on button pressed
#         if (buttonValue==(rv$closeButtonPressed[col.num]+1)) {
#           rv$closeButtonPressed[col.num] = buttonValue
#           resetData(i, input, mainData)
#           rv$panelOrder = rv$panelOrder[rv$panelOrder!=col.num]
#         }
#       }
#     }
#   }
# })


observe({
  
  ## read in data
  mainData <- NULL
  mainData <- .get.data()
  
  # search over all panels
  if (length(rv$panelOrder)>0) {
    
    buttonVals <- list()

    for (i in 1:length(rv$panelOrder)) {       
      
      ## define column selected
      col.num <- rv$panelOrder[i]
      
      # obtain value of action button
      evalString <- paste('input$varClose',col.num,sep='')
      buttonValue <- eval(parse(text=evalString))[1]
      ## replace NULL buttonValues with 0s
      if(is.null(buttonValue)) buttonValue <- 0
      ## set value to 0 if no change from previous count
      if(buttonValue == rv$closeButtonPressed[[col.num]]){
        buttonValue <- 0
      }      
      ## add to list
      buttonVals[[i]] <- buttonValue
    } # end for loop
    
    ## make sure all elements are accounted for, at least as 0s
    for(i in 1:length(buttonVals)){
      if(is.null(buttonVals[[i]])) buttonVals[[i]] <- 0
    }
    ## make it a vector
    buttonVals <- as.vector(unlist(buttonVals))
    
    ## determine whether/ which close button has been clicked
    toClose <- NULL
    if(any(buttonVals > 0)) toClose <- which(buttonVals > 0)
    
    ## on close button pressed
    if(!is.null(toClose)){
      ## get column number for data of panel to be closed
      col.num <- rv$panelOrder[toClose]                  
      ## set rv$closeButtonPressed to reflect the 
      ## CURRENT n.times close has been pressed for column col.num
      #rv$closeButtonPressed[toClose] <- 1
      rv$closeButtonPressed[col.num] <- buttonVals[toClose]
      ## reset data for closing panel
      resetData(toClose, input, mainData)      
      ## NO LONGER SETTING BUTTONS BACK TO 0 (also changed in resetData)
      #rv$closeButtonPressed[toClose] <- 0
      ## remove closed panel from panelOrder
      rv$panelOrder <- rv$panelOrder[-toClose]
    } # end if !is.null(toClose)

  } # end if length(panelOrder)>0
    
}) # end closeButton observer


#######################
## render data table ##
#######################
output$mainDataTable <- renderDataTable({
  rv$subData
},
options=list(scrollX='300px', scrollY='400px', searching=FALSE))



