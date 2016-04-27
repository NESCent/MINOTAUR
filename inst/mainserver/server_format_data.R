

######################
## FORMAT DATA PAGE ##  ------------------------------------------------------------------------------------
######################

######################
## Box: Format Data ##
######################

# box for choosing genomic variables
output$box_formatData <- renderUI({
  box(title="Identify Variables",
      status="primary",
      solidHeader=TRUE,
      collapsible=TRUE,
      width=12,

      h3('Identify Genomic Variables'),

      p('Here you can identify which variables (if any)
        define genomic position and grouping (e.g. chromosome).'),

      fluidRow(
        column(6,
               selectInput('formatData_select_position',
                           label='Position variable',
                           choices=as.list(c('(none)',
                                             names(rawData()$data))),
                           selected=stripPositionChromosome()$posVar,
                           multiple=FALSE)
        ),
        column(6,
               selectInput('formatData_select_chromosome',
                           label='Grouping variable',
                           choices=as.list(c('(none)',
                                             names(rawData()$data))),
                           selected=stripPositionChromosome()$chromVar,
                           multiple=FALSE)
        )
      )
  )
})

## Strip out pos and chrom columns from other variables.
## Return list(posVar,chromVar,otherVar,pos,chrom,y)
stripPositionChromosome <- reactive({

  output <- NULL

  if(!is.null(rawData())){
    if(!is.null(rawData()$data)){
      # extract pos
      posVar <- input$formatData_select_position
      if (is.null(posVar))
        posVar <- '(none)'
      if (posVar=='(none)') {
        pos <- NULL
      } else {
        pos <- rawData()$data[,posVar]
      }

      # extract chrom
      chromVar <- input$formatData_select_chromosome
      if (is.null(chromVar))
        chromVar <- '(none)'
      if (chromVar=='(none)') {
        chrom <- NULL
      } else {
        chrom <- rawData()$data[,chromVar]
        if (length(unique(chrom))>100) {
          errorOn('too_many_chroms')
          chrom <- NULL
        } else {
          errorOff('too_many_chroms')
        }
      }

      # all other variables make up y
      otherVar <- setdiff(names(rawData()$data),c(posVar,chromVar))
      y <- rawData()$data[,otherVar,drop=FALSE]

      # return output
      output <- list(posVar=posVar,
                     chromVar=chromVar,
                     otherVar=otherVar,
                     pos=pos,
                     chrom=chrom,
                     y=y)
    }
  }
  return(output)
})

#######################
## Box: Plot Genomic ##
#######################

# tabBox for producing genomic summary plots
output$tabBox_plotGenomic <- renderUI({
  box(title='Breakdown By Chromosome',
      status="warning",
      solidHeader=TRUE,
      collapsible=FALSE,
      width=12,
      height=300,

      if (is.null(stripPositionChromosome()$chrom)) {
        p('Choose a ',strong('Chromosome variable'),'
          to produce a plot showing the breakdown of observations by chromosome.')
      } else {
        plotOutput('formatData_plot_genomic_observations',height=220)
      }
  )
})

# barplot showing number of observations for each chromosome
output$formatData_plot_genomic_observations <- renderPlot({
  barplot(table(stripPositionChromosome()$chrom),
          col=grey(0.2),
          xlab='Chromosome', ylab='#Observations',
          main='(this will be improved in final version)')
})

######################
## Box: Subset Data ##
######################

# box for choosing genomic variables
output$box_subsetData <- renderUI({
  box(title="Subset Data",
      status="primary",
      solidHeader=TRUE,
      collapsible=TRUE,
      width=12,

      h3('Subset Rows and Columns'),

      p('Here you can limit the variables that you will
        take forward to the outlier detection stage,
        and remove rows containing missing or problematic data.'),

      checkboxInput('formatData_check_takeAllForward',
                    label='Use all remaining variables',
                    value=TRUE),

      conditionalPanel(condition='input.formatData_check_takeAllForward == false',
                       selectizeInput('formatData_selectize_variables',
                                      label='Select specific variables',
                                      choices=stripPositionChromosome()$otherVar,
                                      multiple=TRUE),

                       radioButtons('formatData_radio_removeOrRetain',
                                    label=NULL,
                                    choices=list('remove chosen columns'='remove',
                                                'retain chosen columns'='retain'))
      ),
      p(strong('Remove Missing Data')),
      checkboxGroupInput('formatData_check_removeMissing',
                         label=NULL,
                         choices=list('Remove NA'='NA',
                                      'Remove NaN'='NaN',
                                      'Remove non-finite\n(Inf and -Inf)'='non-finite'))
  )
})

## Reactive conductor for producing final 'clean' data object after removing rows and columns.
## Returns:
# list(pos,
#      pos_modifier,
#      chrom,
#      chromLevels,
#      chromIndex,
#      chromMidpoint,
#      y,
#      pos_userDefined,
#      chrom_userDefined)

cleanData <- reactive({

  output <- NULL

  nullOutput <- list(pos=NULL,
                     pos_modifier=NULL,
                     chrom=NULL,
                     chromLevels=NULL,
                     chromIndex=NULL,
                     chromMidpoint=NULL,
                     y=NULL,
                     pos_userDefined=NULL,
                     chrom_userDefined=NULL)

  data <- stripPositionChromosome()

  if(!is.null(data)){

  # if data$y is NULL, return nullOutput
  if (is.null(data$y))
    # return(nullOutput)
    output <- nullOutput

  # subset columns
  chooseVars <- data$otherVar
  if(!is.null(input$formatData_check_takeAllForward)){
  if (!input$formatData_check_takeAllForward) {
    if (input$formatData_radio_removeOrRetain=='remove') {
      chooseVars <- setdiff(data$otherVar,
                            input$formatData_selectize_variables)
    } else {
      chooseVars <- input$formatData_selectize_variables
    }
  }
  }
  y <- data$y[,chooseVars,drop=FALSE]
  if (ncol(y)==0) # if all columns removed
    # return(nullOutput)
    output <- nullOutput

  ## Use chromosome variable if present,
  ## otherwise temporarily set to 1 everywhere
  if (is.null(data$chrom)) {
    chrom_userDefined <- FALSE
    chrom <- rep(1,nrow(y))
  } else {
    chrom_userDefined <- TRUE
    chrom <- data$chrom
  }

  ## Use position variable if present,
  ## otherwise temporarily set to 1 everywhere
  if (is.null(data$pos)) {
    pos_userDefined <- FALSE
    pos <- rep(1,nrow(y))
  } else {
    pos_userDefined <- TRUE
    pos <- data$pos
  }

  # subset rows
  if (!is.null(input$formatData_check_removeMissing)) {
    df <- cbind(pos,chrom,y)
    keepVec <- rep(1,nrow(y))

    # remove NA
    if ('NA'%in%input$formatData_check_removeMissing)
      keepVec <- keepVec * (rowSums(mapply(is.na,df))==0)

    # remove NaN
    if ('NaN'%in%input$removeMissing)
      keepVec <- keepVec * (rowSums(mapply(function(x){x=='NaN'},df),
                                    na.rm=TRUE)==0)

    # remove non-finite (Inf and -Inf)
    if ('non-finite'%in%input$removeMissing)
      keepVec <- keepVec * (rowSums(mapply(function(x){x%in%c('Inf','-Inf')},df),
                                    na.rm=TRUE)==0)

    # apply all conditions
    pos <- pos[which(keepVec==1)]
    chrom <- chrom[which(keepVec==1)]
    y <- y[which(keepVec==1),,drop=FALSE]
    if (!any(keepVec==1))  # if all rows removed
      # return(nullOutput)
      output <- nullOutput
  }

  # finalise chromosome variable
  if (chrom_userDefined) {
    chromLevels <- unique(as.character(chrom))
    chromIndex <- match(chrom,chromLevels)
  } else {
    chromLevels <- NULL
    chromIndex <- rep(1,nrow(y))
  }

  # finalise position variable - if not user defined increase linearly over chromosomes
  if (pos_userDefined==FALSE) {
    if (chrom_userDefined) {
      chromCounts <- data.frame(table(chrom))
      chromCounts <- chromCounts[match(chromCounts[,1],chromLevels),2]
    } else {
      chromCounts <- nrow(y)
    }
    pos <- sequence(chromCounts)
  }

  # calculate position modifier
  pos_list <- split(pos,chrom)
  pos_maxPerChromosome <- unlist(lapply(pos_list,FUN=function(x){max(x,na.rm=TRUE)}))
  pos_maxIncreasing <- cumsum(pos_maxPerChromosome) - pos_maxPerChromosome
  chromMidpoint <- cumsum(pos_maxPerChromosome) - pos_maxPerChromosome/2
  pos_modifier <- pos_maxIncreasing[chromIndex]

  # return output
  output <- list(pos=pos,
                 pos_modifier=pos_modifier,
                 chrom=chrom,
                 chromLevels=chromLevels,
                 chromIndex=chromIndex,
                 chromMidpoint=chromMidpoint,
                 y=y,
                 pos_userDefined=pos_userDefined,
                 chrom_userDefined=chrom_userDefined)
  }
  # print("str output"); print(str(output))
  return(output)

}) # end cleanData

#################################
## Box: Missing Data % Removed ##
#################################

# valueBox for % missing data removed
#output$valueBox_missingDataRemoved <- renderUI({
#  valueBox(value=HTML(paste('<font size=5>rows removed:  </font> <font size=6>',
#                            finalData()$numRemoved,
#                            ' (',
#                            finalData()$percentRemoved,
#                            '%)</font>'
#                            ,sep='')
#  ), subtitle='', color='yellow', width=12)
#})

########################
## Box: Filtered Data ##
########################

# box for showing filtered data
output$box_finalData <- renderUI({
  box(title="Final Data",
      status="warning",
      solidHeader=TRUE,
      collapsible=TRUE,
      width=12,

      dataTableOutput("table_finalData")
  )
})

# filtered data table
output$table_finalData <- renderDataTable({

  # if cleanData()$y is NULL, return NULL
  if (is.null(cleanData()$y))
    return(NULL)

  df <- data.frame(Position=cleanData()$pos,
                   Chromosome=cleanData()$chrom)
  df <- cbind(df,cleanData()$y)
  return(df)
},options=list(scrollX=TRUE, scrollY='500px') #, rownames=FALSE
)


