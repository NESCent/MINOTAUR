
########################
## FIND OUTLIERS PAGE ##  ------------------------------------------------------------------------------------
########################

#######################################################
## Box: Global Controls for Univariate Distributions ##
#######################################################

# box containing controls for univariate distribution plots
output$box_global_univariate <- renderUI({
  box(title="Univariate Distributions", status="primary", solidHeader=TRUE, collapsible=TRUE, width=12,
      h3('Combine Univariate Distributions to Detect Outliers'),
      p('Here you can examine and compare the univariate distributions of each of your variables. In the panels below you can combine these univariate measures to arrive at various multivariate distances, which in turn can be used to identify outliers.'),

      fluidRow(
        column(6,
               selectInput('positionVariable', label='Position variable', choices=as.list(c('(none)',names(finalData()$data))))
        ),
        column(6,
               selectInput('chromosomeVariable', label='Chromosom variable', choices=as.list(c('(none)',names(finalData()$data))))
        )
      ),
      fluidRow(
        column(12,
               selectizeInput('univariateVariables', label='Plotting variable(s)', choices=names(finalData()$data), multiple=TRUE)
        )
      )
  )
})

# reactive conductor for taking finalData() and extracting values defined in box_global_univariate. Returns list(x,y,chrom,chromLevels,chromIndex)
outlierData <- reactive({
  nullOutput <- list(x=NULL, y=NULL, chrom=NULL, chromLevels=NULL, chromIndex=NULL)
  finalData <- finalData()

  # if finalData$data is NULL, return nullOutput
  if (is.null(finalData$data))
    return(nullOutput)

  # if univariateVariables is NULL, return nullOutput
  if (is.null(input$univariateVariables))
    return(nullOutput)

  # find out whether using position and/or chromosome variables
  usePosition <- FALSE
  useChromosome <- FALSE
  if (!is.null(input$positionVariable))
    usePosition <- (input$positionVariable!='(none)')
  if (!is.null(input$chromosomeVariable))
    useChromosome <- (input$chromosomeVariable!='(none)')

  # use chromosome variable if present, otherwise chrom=NULL. If number of chromosomes is too large (>100) throw an error
  if (useChromosome) {
    chrom <- finalData$data[,input$chromosomeVariable]
    chromLevels <- unique(as.character(chrom))
    chromIndex <- match(chrom,chromLevels)
    if (length(chromLevels)>100) {
      errorOn('too_many_chroms')
      return(nullOutput)
    }
  } else {
    chrom <- NULL
    chromLevels <- NULL
    chromIndex <- NULL
  }
  errorOff('too_many_chroms')

  # use position variable if present, increase linearly over chromosomes if not
  if (usePosition) {
    x <- finalData$data[,input$positionVariable]
  } else {
    if (useChromosome) {
      chromCounts <- data.frame(table(chrom))
      chromCounts <- chromCounts[match(chromCounts[,1],chromLevels),2]
    } else {
      chromCounts <- finalData$rows
    }
    x <- sequence(chromCounts)
  }

  # get variable(s) to plot on y-axis
  y <- finalData$data[,input$univariateVariables,drop=FALSE]

  # output values
  output <- list(x=x,
                 y=y,
                 chrom=chrom,
                 chromLevels=chromLevels,
                 chromIndex=chromIndex)
  return(output)
})

##########################
## Box: Navigation Plot ##
##########################

# box for navigation plot
output$box_navigation <- renderUI({
  tabBox(title="Navigation", width=12,
         tabPanel(title=NULL,
                  selectInput('restrict_chrom', label='Restrict chromosome', choices=as.list(c('(all)',outlierData()$chromLevels)), selected=outlierData_sub()$chromChosen, width=200),
                  plotOutput('plot_navigation',height=100,width='100%'),
                  sliderInput('slider_navigate',
                              label=NULL,
                              min=outlierData_sub()$x_min,
                              max=outlierData_sub()$x_max,
                              value=c(outlierData_sub()$x_min,outlierData_sub()$x_max),
                              step=1,
                              width='100%'),
                  div(style="display:inline-block",
                      tags$button(id='apply_navigate_button', type="button",
                                  class="btn action-button btn-primary",
                                  style='font-size:15px; text-align:center',
                                  HTML('<i class="icon-star"></i>Apply Navigation'))
                  )
         ) # end of tabPanel
  ) # end of tabBox
})

# reactive conductor for subsetting outlierData based on restrict_chrom selector. Returns list(x,x_modifier,x_min,x_max,y,y_cols,chrom,chromLevels,chromIndex,chromPos,chromChosen)
outlierData_sub <- reactive({
  nullOutput <- list(x=NULL, x_modifier=NULL, x_min=0, x_max=100, y=NULL, y_cols=1, chrom=NULL, chromLevels=NULL, chromIndex=NULL, chromPos=NULL, chromChosen='(all)')
  outlierData <- outlierData()

  # if outlierData$x is NULL, return nullOutput
  if (is.null(outlierData$x))
    return(nullOutput)

  # if input$restrict_chrom is NULL, return nullOutput
  if (is.null(input$restrict_chrom))
    return(nullOutput)

  # if plotting all chromosomes
  if (input$restrict_chrom=='(all)') {
    x <- outlierData$x
    y <- outlierData$y
    chrom <- outlierData$chrom
    chromLevels <- outlierData$chromLevels
    chromIndex <- outlierData$chromIndex

    if (is.null(chrom)) {
      x_modifier <- 0
      chromPos <- NULL
    } else {
      x_list <- split(x,chrom)
      x_maxPerChromosome <- unlist(lapply(x_list,FUN=function(x){max(x,na.rm=TRUE)}))
      x_maxIncreasing <- cumsum(x_maxPerChromosome) - x_maxPerChromosome
      chromPos <- cumsum(x_maxPerChromosome) - x_maxPerChromosome/2
      x_modifier <- x_maxIncreasing[chromIndex]
    }

    output <- list(
      x=x,
      x_modifier=x_modifier,
      x_min=min(x+x_modifier,na.rm=TRUE),
      x_max=max(x+x_modifier,na.rm=TRUE),
      y=y,
      y_cols=ncol(y),
      chrom=chrom,
      chromLevels=chromLevels,
      chromIndex=chromIndex,
      chromPos=chromPos,
      chromChosen='(all)')

  # if restricted to single chromosome
  } else {

    x <- subset(outlierData$x, outlierData$chrom==input$restrict_chrom)
    y <- subset(outlierData$y, outlierData$chrom==input$restrict_chrom)
    chromIndex <- subset(outlierData$chromIndex, outlierData$chrom==input$restrict_chrom)

    output <- list(
      x=x,
      x_modifier=rep(0,length(x)),
      x_min=min(x,na.rm=TRUE),
      x_max=max(x,na.rm=TRUE),
      y=y,
      y_cols=ncol(y),
      chrom=input$restrict_chrom,
      chromLevels=input$restrict_chrom,
      chromIndex=chromIndex,
      chromPos=median(x,na.rm=TRUE),
      chromChosen=input$restrict_chrom)
  }

  # return output
  return(output)
})


# reactive conductor for converting outlierData_sub() into series of polygons for navigation plot. Returns list(x_min,x_max,y_min,y_max,polygon=list(x,y,chromIndex),chromLevels,chromPos)
navigationPolygons <- reactive({
  nullOutput <- list(x_min=0,x_max=100,y_min=0,y_max=1,polygon=list(),chromLevels=NULL,chromPos=NULL)
  outlierData_sub <- outlierData_sub()

  # if outlierData_sub$x is NULL, return nullOutput
  if (is.null(outlierData_sub$x))
    return(nullOutput)

  # convert x and y to polygons
  polyNum <- 200
  x <- outlierData_sub$x + outlierData_sub$x_modifier
  x_min <- outlierData_sub$x_min
  x_max <- outlierData_sub$x_max
  y <- outlierData_sub$y[,1]
  y_min <- min(y,na.rm=TRUE)
  y_max <- max(y,na.rm=TRUE)

  chromIndex <- outlierData_sub$chromIndex
  if (is.null(chromIndex)) {
    df <- data.frame(y=y, chromIndex=1)
  } else {
    df <- data.frame(y=y, chromIndex=chromIndex)
  }
  breakVec <- seq(x_min, x_max, l=polyNum+1)
  breakDelta <- (breakVec[2]-breakVec[1])/2
  breakMids <- (breakVec[-1]+breakVec[-length(breakVec)])/2
  c <- split(df,f=cut(x,breaks=breakVec))  # split df into list based on which interval x falls into
  output <- list(x_min=x_min,
                 x_max=x_max,
                 y_min=y_min,
                 y_max=y_max,
                 polygon=list(),
                 chromLevels=outlierData_sub$chromLevels,
                 chromPos=outlierData_sub$chromPos)
  for (i in 1:length(c)) {
    if (length(c[[i]]$y)>0) {
      r <- range(c[[i]]$y,na.rm=TRUE)
      output$polygon[[i]] <- list(x=c(breakMids[i]-breakDelta, breakMids[i]+breakDelta, breakMids[i]+breakDelta, breakMids[i]-breakDelta),
                          y=c(r[1],r[1],r[2],r[2]), chromIndex=c[[i]]$chromIndex[1])
    }
  }
  return(output)
})

# navigation plot
output$plot_navigation <- renderPlot({
  polys <- navigationPolygons()

  # if no polygons, plot placeholder
  if (length(polys$polygon)==0) {
    par(mar=c(0.2,0.8,0.2,0.8))
    plot(0, type='n', xlim=c(0,1), ylim=c(0,1), xaxs='i', yaxs='i', axes=FALSE)
    text(0.5,0.5,'(Select plotting variables)',cex=1.5)
    return()
  }

  # produce empty plot
  par(mar=c(0.2,0.8,0.2,0.8), xpd=NA)
  k <- 0.8
  ylim <- c(polys$y_min, polys$y_max*(0.5+k)+polys$y_min*(0.5-k))
  plot(0, type='n', xlim=c(polys$x_min,polys$x_max), ylim=ylim, xaxs='i', yaxs='i', axes=FALSE)

  # add text numbering chromosomes
  if (!is.null(polys$chromPos)) {
    text(x=polys$chromPos, y=polys$y_max*(0.5+0.7)+polys$y_min*(0.5-0.7), labels=polys$chromLevels, font=2)
  }

  # add polygons, including outside border
  for (i in 1:length(polys$polygon)) {
    polygon(polys$polygon[[i]]$x, polys$polygon[[i]]$y, col=c('black','red')[2-polys$polygon[[i]]$chromIndex%%2], border='white')
  }
  polygon(c(polys$x_min,polys$x_max,polys$x_max,polys$x_min), c(polys$y_min,polys$y_min,polys$y_max,polys$y_max))

  # grey-out side regions based on slider
  s <- input$slider_navigate
  polygon(c(polys$x_min,s[1],s[1],polys$x_min),
          c(polys$y_min,polys$y_min,polys$y_max,polys$y_max),
          col=rgb(1,1,1,0.8))
  polygon(c(polys$x_max,s[2],s[2],polys$x_max),
          c(polys$y_min,polys$y_min,polys$y_max,polys$y_max),
          col=rgb(1,1,1,0.8))
})


###########################################################
## Box: Individual Controls for Univariate Distributions ##
###########################################################

# box containing individual controls for univariate distributions
output$box_controls_univariate <- renderUI({
  box(title="", solidHeader=TRUE, collapsible=TRUE, width=12,
      uiOutput('lapply_controls')
  )
})

output$lapply_controls <- renderUI({
  k <- 1
  lapply(1:k,function(i){
    selectInput(paste('foo_',i,sep=''), label=paste('bar',i), choices=list(i))
  })
})


###########################
## Box: Univariate Plots ##
###########################

# box for plotting univariate distributions
output$box_plot_univariate <- renderUI({
  tabBox(title="Univariate Plots", width=12,
         tabPanel(title=NULL,
                  plotOutput('plot1',height=400*outlierData_sub()$y_cols)
         )
  )
})

# function evaluates when apply_navigate_button is pressed
apply_navigate <- eventReactive(input$apply_navigate_button,{})

# plot univariate distributions
output$plot1 <- renderPlot({

  # only evaluate when apply_navigate_button is pressed
  apply_navigate()

  # isolate data used in plotting
  outlierData_sub <- isolate(outlierData_sub())

  # if outlierData_sub$x is NULL, return NULL
  if (is.null(outlierData_sub$x))
    return(NULL)

  # isolate slider info
  xlim <- isolate(c(input$slider_navigate[1], input$slider_navigate[2]))

  # plot chromosomes in alternating black and red
  if (is.null(outlierData_sub$chromIndex)) {
    colVec <- 'black'
  } else {
    colVec <- c('black','red')[2-outlierData_sub$chromIndex%%2]
  }

  # produce univariate plots
  k <- outlierData_sub$y_cols
  mar_top <- 1/(k+2)
  mar_bot <- 1/(k+2)
  mid <- 1-mar_top-mar_bot
  for (i in 1:k) {
    plot_top <- mar_bot + mid*(k-i+1)/k
    plot_bot <- mar_bot + mid*(k-i)/k
    par(fig=c(0,1,plot_bot,plot_top),
        mar=c(0,4.1,0,2.1),
        new=i!=1)
    plot(outlierData_sub$x+outlierData_sub$x_modifier, outlierData_sub$y[,i], col=colVec, pch=20, cex=1, xaxs='i', xlim=xlim)
  }

})

##############################################
## Box: Controls for Distance-Based Methods ##
##############################################

# box containing controls for distance-based measure plots
output$box_control_distanceBased <- renderUI({
  box(title="Distance-Based Outlier Detection", status="warning", solidHeader=TRUE, collapsible=TRUE, width=4,
      h3('something in here')
  )
})

###############################
## Box: Distance-Based Plots ##
###############################

# box for distance-based measures (harmonic mean and nearest-neighbour)
output$box_plot_distanceBased <- renderUI({
  tabBox(title='Distance-Based Methods', width=8,
         tabPanel('Harmonic Mean Distance','',
                  plotOutput('plot2')
         ),
         tabPanel('Nearest-Neighbour Distance','',
                  p('bloo blah')
         )
  )
})

# example plot
output$plot2 <- renderPlot({
  plot(rnorm(1e3))
})

#############################################
## Box: Controls for Density-Based Methods ##
#############################################

# box containing controls for density-based measure plots
output$box_control_densityBased <- renderUI({
  box(title="Density-Based Outlier Detection", status="warning", solidHeader=TRUE, collapsible=TRUE, width=4,
      p('this is where I put the details of density-based measures, such as kernel-density with fixed and ML bandwidth.')
  )
})

##############################
## Box: Density-Based Plots ##
##############################

# box for density-based measures (kernel density)
output$box_plot_densityBased <- renderUI({
  tabBox(title="Density-Based Methods", width=8,
         tabPanel('Heuristic Bandwidth','',
                  plotOutput('plot3')
         ),
         tabPanel('Maximum-Likelihood Bandwidth','',
                  p('begin trick')
         )
  )
})

# example plot
output$plot3 <- renderPlot({
  plot(rnorm(1e3))
})


#############################
## (RANDOM CRAP, NOT USED) ##
#############################

#### attempt at ggvis
#df1 <- data.frame(x=1:100,y=rnorm(100))
#df2 <- reactive({
#  if (is.null(input$size)) {
#    df <- data.frame(x=rep(0,2),y=c(-10,10))
#  } else {
#    df <- data.frame(x=rep(input$size,2),y=c(-10,10))
#  }
#})
##  return(df)
#df1 %>% ggvis(~x, ~y) %>% layer_points(size:=5) %>% layer_paths(data=df2,x=~x,y=~y,strokeWidth:=input_slider(1,10)) %>% bind_shiny('p','p_ui')

#### using brushes
#plotOutput('plot_navigation',height=100,
#           brush=brushOpts(
#             id='navigation_brush',
#             direction='x'
#           )
#),
