

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
      
      #### NOTE - PERFORM CHECKS THAT POS AND GROUP VARIABLES ARE SENSIBLE AT THIS STAGE. ERROR MESSAGE IF NOT.
      
      # extract position variable
      posVar <- input$formatData_select_position
      if (is.null(posVar))
        posVar <- '(none)'
      if (posVar=='(none)') {
        pos <- NULL
      } else {
        pos <- rawData()$data[,posVar]
      }

      # extract grouping variable
      chromVar <- input$formatData_select_chromosome
      if (is.null(chromVar))
        chromVar <- '(none)'
      if (chromVar=='(none)') {
        chrom <- NULL
      } else {
        chrom <- rawData()$data[,chromVar]
        #if (length(unique(chrom))>100) { #### REPLACE WITH ERROR IN CONDITIONAL PANEL
        #  errorOn('too_many_chroms')
        #  chrom <- NULL
        #} else {
        #  errorOff('too_many_chroms')
        #}
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
  box(title='Breakdown By Group',
      status="warning",
      solidHeader=TRUE,
      collapsible=TRUE,
      width=12,
      height=300,

      if (is.null(stripPositionChromosome()$chrom)) {
        p('Choose a ',strong('Grouping variable'),'
          (for example grouping by chromosome) to produce a plot showing the breakdown of observations by group')
      } else {
        # if too many unique groups then give warning message
        if (length(unique(stripPositionChromosome()$chrom))>100) {
          p('(Grouping variable contains too many unique levels to plot)')
        } else {
          p('UNDER CONSTRUCTION')
          #plotOutput('formatData_plot_genomic_observations',height=220)
        }
      }
  )
})

# barplot showing number of observations for each chromosome
#output$formatData_plot_genomic_observations <- renderPlot({
#  groupingVar <- stripPositionChromosome()$chrom
#  uniques <- length(unique(groupingVar))
#  
#  barplot(table(groupingVar),
#          col=grey(0.2),
#          xlab='Chromosome', ylab='#Observations',
#          main='(this will be improved in final version)')
#})

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
# list(posVar=posVar, <- which variable defines the position
#      chromVar=chromVar,  <- which variable defines the group (chromosome)
#      otherVar=otherVar,  <- which variables define all other chosen columns
#      pos,  <- vector of positions
#      pos_modifier,  <- modifier to make positions increasing over chromosomes (for plotting)
#      chrom,  <- vector of raw groupings, could be factors
#      chromLevels, <- vector of unique levels of grouping variable
#      chromIndex,  <- same as chrom but converted to integer that indexes value in chromlevels
#      chromMidpoint,  <- midpoint of all pos in each group (for plotting)
#      y,  <- data
#      pos_userDefined, <- whether pos is user-defined or default
#      chrom_userDefined) <- whether chrom is user-defined or default
cleanData <- reactive({
  
  # define null output to return if some sort of error. Ensures that output format is maintained even in event of an error.
  nullOutput <- list(posVar=NULL,
                     chromVar=NULL,
                     otherVar=NULL,
                     pos=NULL,
                     pos_modifier=NULL,
                     chrom=NULL,
                     chromLevels=NULL,
                     chromIndex=NULL,
                     chromMidpoint=NULL,
                     y=NULL,
                     pos_userDefined=NULL,
                     chrom_userDefined=NULL)

  # initialise output as null, then fill in elements one at a time
  output <- nullOutput
  
  # read in data from previous step
  data <- stripPositionChromosome()

  if(!is.null(data)) {

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
  output$y <- data$y[,chooseVars,drop=FALSE]
  if (ncol(output$y)==0) # if all columns removed
    return(nullOutput)
  output$posVar <- data$posVar
  output$chromVar <- data$chromVar
  output$otherVar <- chooseVars

  ## Use chromosome variable if present,
  ## otherwise temporarily set to 1 everywhere
  if (is.null(data$chrom)) {
    output$chrom_userDefined <- FALSE
    output$chrom <- rep(1,nrow(output$y))
  } else {
    output$chrom_userDefined <- TRUE
    output$chrom <- data$chrom
  }

  ## Use position variable if present,
  ## otherwise temporarily set to 1 everywhere
  if (is.null(data$pos)) {
    output$pos_userDefined <- FALSE
    output$pos <- rep(1,nrow(output$y))
  } else {
    output$pos_userDefined <- TRUE
    output$pos <- data$pos
  }

  # subset rows
  if (!is.null(input$formatData_check_removeMissing)) {
    df <- cbind(output$pos, output$chrom, output$y)
    keepVec <- rep(1,nrow(output$y))

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
    output$pos <- output$pos[which(keepVec==1)]
    output$chrom <- output$chrom[which(keepVec==1)]
    output$y <- output$y[which(keepVec==1),,drop=FALSE]
    if (!any(keepVec==1))  # if all rows removed
      output <- nullOutput
  }

  # finalise chromosome variable
  if (output$chrom_userDefined) {
    output$chromLevels <- unique(as.character(output$chrom))
    output$chromIndex <- match(output$chrom, output$chromLevels)
  } else {
    output$chromLevels <- NULL
    output$chromIndex <- rep(1,nrow(output$y))
  }

  # finalise position variable - if not user defined increase linearly over chromosomes
  if (output$pos_userDefined==FALSE) {
    if (output$chrom_userDefined) {
      chromCounts <- data.frame(table(output$chrom))
      chromCounts <- chromCounts[match(chromCounts[,1], output$chromLevels),2]
    } else {
      chromCounts <- nrow(output$y)
    }
    output$pos <- sequence(chromCounts)
  }

  # calculate position modifier
  pos_list <- split(output$pos, output$chrom)
  pos_maxPerChromosome <- as.numeric(unlist(lapply(pos_list,FUN=function(x){max(x,na.rm=TRUE)})))
  pos_maxIncreasing <- cumsum(pos_maxPerChromosome) - pos_maxPerChromosome
  output$chromMidpoint <- cumsum(pos_maxPerChromosome) - pos_maxPerChromosome/2
  output$pos_modifier <- pos_maxIncreasing[output$chromIndex]
  
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

  #df <- data.frame(Position_variable=cleanData()$pos,
  #                 Grouping_variable=cleanData()$chrom)
  #df <- cbind(df,cleanData()$y)
  df <- data.frame(x=1:5,y=1:5)
  return(df)
},options=list(scrollX=TRUE, scrollY='400px') #, rownames=FALSE
)


