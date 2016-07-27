

######################
## FORMAT DATA PAGE ##  ------------------------------------------------------------------------------------
######################


######################
## Box: Format Data ##
######################

# box for choosing genomic variables
output$box_formatData <- renderUI({

  chromVarSel <- posVarSel <- NULL
  chromVarSel <- stripPositionChromosome()$chromVar
  posVarSel <- stripPositionChromosome()$posVar


  box(title="Identify Variables",
      status="primary",
      solidHeader=TRUE,
      collapsible=TRUE,
      width=12,

      h3('Identify Genomic Variables'),

      p('Here you can identify which variables (if any)
        define genomic position (e.g., base pair) and grouping (e.g., chromosome).'),

      fluidRow(
        column(6,
               selectInput('formatData_select_position',
                           label='Position variable',
                           choices=as.list(c('(none)',
                                             names(rawData()$data))),
                           selected=posVarSel,
                           multiple=FALSE)
        ),
        column(6,
               selectInput('formatData_select_chromosome',
                           label='Grouping variable',
                           choices=as.list(c('(none)',
                                             names(rawData()$data))),
                           selected=chromVarSel,
                           multiple=FALSE)
        )
      )
      )

})

## Strip out pos and chrom columns from other variables. pos is coerced to numeric and chrom is coerced to character.
## Return list(posVar,chromVar,otherVar,pos,chrom,y)
stripPositionChromosome <- reactive({

  output <- NULL

  if(!is.null(rawData())){
    if(!is.null(rawData()$data)){

      # extract position variable
      posVar <- input$formatData_select_position
      if (is.null(posVar))
        posVar <- '(none)'
      if (posVar=='(none)') {
        pos <- NULL
      } else {
        ## Reset selected position variable (if dataset has changed)
        if(!posVar %in% names(rawData()$data)){
          pos <- NULL
        }else{
        pos <- rawData()$data[,posVar]
        if (!is.numeric(pos))
          pos <- as.numeric(as.factor(pos))
        }
      }

      # extract grouping variable
      chromVar <- input$formatData_select_chromosome
      if (is.null(chromVar))
        chromVar <- '(none)'
      if (chromVar=='(none)') {
        chrom <- NULL
      } else {
        ## Reset selected chromosome variable (if dataset has changed)
        if(!chromVar %in% names(rawData()$data)){
          chrom <- NULL
        }else{
            chrom <- as.character(rawData()$data[,chromVar])
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

#########################
## Box: Plot Breakdown ##
#########################

# box for plotting breakdown by grouping variable
output$box_plotBreakdown <- renderUI({
  box(title='Breakdown By Group',
      status="warning",
      solidHeader=TRUE,
      collapsible=TRUE,
      width=12,
      #height=300,

      if (is.null(stripPositionChromosome()$chrom)) {
        p('Choose a ',strong('Grouping variable'),'
          (for example grouping by chromosome) from the menu in the panel at left
          to produce a plot showing the breakdown of observations by group.')
      } else {
        # if too many unique groups then give warning message
        if (length(unique(stripPositionChromosome()$chrom))>100) {
          p('(Grouping variable contains too many unique levels to plot.)')
        } else {
          plotOutput('formatData_plot_genomic_observations',height=300)
        }
      }
  )
})

# barplot showing number of observations for each chromosome
output$formatData_plot_genomic_observations <- renderPlot({
  posVar <- stripPositionChromosome()$pos # this is NULL or a numeric vector
  groupingVar <- stripPositionChromosome()$chrom  # this is NULL or a character vector

  if (!is.null(groupingVar)) {
    uniques <- unique(groupingVar)
    if (length(uniques)<100) {
      if(!is.null(posVar)){

        # split pos by group
        pos_list <- split(posVar, groupingVar)
        pos_range <- matrix(as.numeric(unlist(lapply(pos_list, FUN=function(x){range(x,na.rm=TRUE)}))), ncol=2, byrow=T)
        minVal <- min(pos_range)
        maxVal <- max(pos_range)

        # print empty plot
        par(fig=c(0,1,0.85,1),mar=c(0,0,0,0))
        plot(0,type='n',xlim=c(-1,1),ylim=c(-1,1),axes=FALSE,ann=FALSE)
        text(0,0,'genomic coverage broken down by group',cex=1.2,font=2)

        par(fig=c(0.1,0.9,0.1,0.85),mar=c(0,0,0,0),new=TRUE)
        y_pretty <- pretty(pos_range)
        plot(0,type='n', xlim=c(0,length(uniques)), ylim=range(y_pretty), xaxs='i', yaxs='i', axes=F)
        axis(1,at=1:length(uniques)-0.5,labels=1:length(uniques))
        axis(2,at=y_pretty)

        # loop through all groups
        myPal <- colorRampPalette(c('white','red'))
        for (i in 1:length(uniques)) {
          x_range <- c((i-1+0.1)/length(uniques), (i-0.1)/length(uniques))
          x_range <- x_range*(1-0.1-0.1) + 0.1
          y_range <- c((pos_range[i,1]-minVal)/(maxVal-minVal), (pos_range[i,2]-minVal)/(maxVal-minVal))
          y_range <- y_range*(1-0.1-0.15) + 0.1
          if (y_range[1]!=y_range[2]) {

            # add image plot based on sampling density
            m <- matrix(table(findInterval(pos_list[[i]],seq(pos_range[i,1],pos_range[i,2],l=101), rightmost.closed=T)),1)
            par(fig=c(x_range[1], x_range[2], y_range[1], y_range[2]),mar=c(0,0,0,0),new=T)
            image(m,axes=F,col=myPal(10))

            # add border
            par(fig=c(0,1,0,1),mar=c(0,0,0,0),new=T)
            plot(0,type='n', xlim=c(0,1), ylim=c(0,1), xaxs='i', yaxs='i', axes=F, ann=F)
            polygon(c(x_range[1],x_range[2],x_range[2],x_range[1]), c(y_range[1],y_range[1],y_range[2],y_range[2]))
          }
        }
      } else {

        # extract number of times each unique group is represented
        tab <- data.frame(table(groupingVar))
        uniques <- unique(groupingVar)
        counts <- tab[match(uniques,tab[,1]),2]

        # produce plot
        barplot(counts, names=1:length(uniques), col='black', main='number of observations in each group')
      }
    }
  }
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
                                      'Remove non-finite\n(Inf and -Inf)'='non-finite'),
                          selected=c('NA','NaN','non-finite'))
      )
})

## Reactive conductor for producing final 'clean' data object after removing rows and columns.
## Returns:
# list(posVar=posVar, <- which variable defines the position
#      chromVar=chromVar,  <- which variable defines the group (chromosome)
#      otherVar=otherVar,  <- which variables define all other chosen columns
#      pos,  <- vector of positions. Coerced to numeric
#      pos_modifier,  <- modifier to make positions increasing over chromosomes (for plotting)
#      chrom,  <- vector of raw groupings. Could be factors
#      chromLevels, <- vector of unique levels of chrom
#      chromIndex,  <- same as chrom but converted to integer that indexes value in chromlevels
#      chromMidpoint,  <- midpoint of all pos in each group (for plotting)
#      y,  <- data    <- the actual data
#      numRemoved,    <- number of rows removed due to missing data
#      percentRemoved,    <- percentage rows removed due to missing data
#      pos_userDefined, <- whether pos is user-defined or default
#      chrom_userDefined) <- whether chrom is user-defined or default
cleanData <- reactive({

  ## define null output to return if some sort of error.
  ## Ensures that output format is maintained even in event of an error.
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
                     numRemoved=NULL,
                     percentRemoved=NULL,
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
      output$chrom <- as.character(rep(1,nrow(output$y)))
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
      df <- data.frame(output$pos, output$chrom, output$y)
      df[,2] <- as.character(df[,2])
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
      output$numRemoved <- sum(keepVec!=1)
      output$percentRemoved <- round(output$numRemoved/length(keepVec)*100,2)
    }

    # finalise chromosome variable
    if (output$chrom_userDefined) {
      output$chromLevels <- unique(output$chrom)
      output$chromIndex <- match(output$chrom, output$chromLevels)
    } else {
      output$chromLevels <- NULL
      output$chromIndex <- rep(1,nrow(output$y))
    }

    # finalise position variable - if not user defined increase linearly over chromosomes
    if (output$pos_userDefined==FALSE) {
      if (output$chrom_userDefined) {
        chromCounts <- data.frame(table(output$chromIndex))$Freq
      } else {
        chromCounts <- nrow(output$y)
      }
      output$pos <- sequence(chromCounts)
    }

    # calculate position modifier. pos+pos_modifier gives a sequence that increases over the entire genome
    pos_list <- split(output$pos, output$chrom)
    pos_maxPerChromosome <- as.numeric(unlist(lapply(pos_list,FUN=function(x){max(x,na.rm=TRUE)})))
    pos_maxIncreasing <- cumsum(pos_maxPerChromosome) - pos_maxPerChromosome
    output$chromMidpoint <- cumsum(pos_maxPerChromosome) - pos_maxPerChromosome/2
    output$pos_modifier <- pos_maxIncreasing[output$chromIndex]
  }

  return(output)
}) # end cleanData

#################################
## Box: Missing Data % Removed ##
#################################

# valueBox for % missing data removed
output$valueBox_missingDataRemoved <- renderUI({
  shinydashboard::valueBox(value=HTML(paste('<font size=5>rows removed:  </font> <font size=6>',
                            cleanData()$numRemoved,
                            ' (',
                            cleanData()$percentRemoved,
                            '%)</font>'
                            ,sep='')
  ), subtitle='', color='yellow', width=12)
})

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

  # otherwise return data frame
  df <- data.frame(Position_variable=cleanData()$pos,
                   Grouping_variable=cleanData()$chrom)
  df <- cbind(df,cleanData()$y)
  return(df)
},options=list(scrollX=TRUE, scrollY='400px') #, rownames=FALSE
)


