
########################
## FIND OUTLIERS PAGE ##  ------------------------------------------------------------------------------------
########################

#######################################################
## Box: Global Controls for Univariate Distributions ##
#######################################################

# box containing controls for univariate distribution plots
output$box_global_univariate <- renderUI({
  box(title="Univariate Distributions", status="primary", solidHeader=TRUE, collapsible=TRUE, width=12,
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
               selectizeInput('univariateVariables', label='Plotting variable(s)', choices=names(finalData()$data), multiple=TRUE),
               useShinyjs(),
               disabled(
                 selectInput('foobar', label='foobar', choices=list('foo','bar'))
               )
        )
      )
  )
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
  k <- 3
  lapply(1:k,function(i){
    selectInput(paste('foo_',i,sep=''), label=paste('bar',i), choices=list(i))
  })
})

##########################
## Box: Navigation Plot ##
##########################

# box for navigation plot
output$box_navigation <- renderUI({
  tabBox(title="Navigation", width=12,
         tabPanel('(some text)','',
                  #plotOutput('plot_navigation',height=100,
                  #           brush=brushOpts(
                  #             id='navigation_brush',
                  #             direction='x'
                  #           )
                  #),
                  ggvisOutput('ggvis'),
                  uiOutput('ggvis_ui'),
                  sliderInput('size','Area',value=20,10,1000),
                  sliderInput('slider_navigate',
                              label=NULL,
                              min=navigationPlotParameters()$x_min,
                              max=navigationPlotParameters()$x_max,
                              value=c(navigationPlotParameters()$x_min, navigationPlotParameters()$x_max),
                              step=1,
                              width='100%')
         ) # end of tabPanel
  ) # end of tabBox
})

input_size <- reactive(input$size)

#get_rct_df %>% ggvis() %>% layer_points(x=~x,y=~y) %>% bind_shiny('plot')
mtcars %>% ggvis(~wt, ~mpg, size:=2) %>% layer_points() %>% bind_shiny('ggvis','ggvis_ui')

# reactive conductor for calculating plotting parameters. Returns list(x,x_modifier,y,colVec,x_min,x_max)
navigationPlotParameters <- reactive({
  nullOutput <- list(x=NULL, x_modifier=NULL, y=NULL, colVec=NULL, x_min=0, x_max=100)
  finalData <- finalData()

  # if finalData$data is NULL, return nullOutput
  if (is.null(finalData$data))
    return(nullOutput)

  # if univariateVariables is NULL, return nullOutput
  if (is.null(input$univariateVariables))
    return(nullOutput)

  # find if using position and chromosome variables
  usePosition <- FALSE
  useChromosome <- FALSE
  if (!is.null(input$positionVariable))
    usePosition <- (input$positionVariable!='(none)')
  if (!is.null(input$chromosomeVariable))
    useChromosome <- (input$chromosomeVariable!='(none)')

  # use chromosome variable if present, 1 otherwise
  if (useChromosome) {
    chrom <- finalData$data[,input$chromosomeVariable]
  } else {
    chrom <- rep(1,finalData$rows)
  }

  # use position variable if present, increase linearly over chromosomes if not
  if (usePosition) {
    x <- finalData$data[,input$positionVariable]
  } else {
    chromCounts <- data.frame(table(chrom))
    chromCounts <- chromCounts[match(chromCounts[,1],unique(chrom)),2]
    x <- sequence(chromCounts)
  }

  # calculate position modifier, which ensures that positions increase continuously over genome
  x_list <- split(x,chrom)
  x_maxPerChromosome <- unlist(lapply(x_list,max))
  x_maxIncreasing <- cumsum(x_maxPerChromosome) - x_maxPerChromosome
  x_modifier <- x_maxIncreasing[match(chrom,unique(chrom))]

  # get variable to plot on y-axis
  y <- finalData$data[,input$univariateVariables[1]]

  # convert chromosomes to colours
  colVec <- c(grey(0.8),'#FF9999')[2-chrom%%2]

  # output final parameters
  output <- list(x=x,
                 x_modifier=x_modifier,
                 y=y,
                 colVec=colVec,
                 x_min=min(x+x_modifier),
                 x_max=max(x+x_modifier))
  return(output)
})

# reactive conductor for producing basic navigation plot image. Returns ggplot2 object.
navigationImage <- reactive({
  print('PRODUCING RAW IMAGE')
  params <- navigationPlotParameters()

  if (is.null(params$x))
    return(NULL)

  p <- ggplot(data.frame(x=params$x+params$x_modifier, y=params$y))
  p <- p + theme_bw()
  p <- p + geom_point(aes(x=x, y=y))

  return(p)
})


# navigation plot
output$plot_navigation <- renderPlot({
  print('PRODUCING NAVPLOT')

  navParams <- navigationPlotParameters()
  if (is.null(navParams$x))
    return(NULL)

  enable('foobar')

  # produce navigation plot
  par(mar=c(0.2,0.2,0.2,0.2))
  p1 <- navigationImage()
  p2 <- p1 + geom_vline(xintercept=input$slider_navigate[1])
  plot(5)

  #plot(navParams$x+navParams$x_modifier, navParams$y, col=navParams$colVec, pch=20, cex=0.7, xaxs='i', yaxs='i', xaxt='n', yaxt='n', ann=FALSE)
  #abline(v=input$slider_navigate)
})

###########################
## Box: Univariate Plots ##
###########################

# box for plotting univariate distributions
output$box_plot_univariate <- renderUI({
  tabBox(title="Univariate Plots", width=12,
         tabPanel('(some text)','',
                  plotOutput('plot1'),
                  h3(input$navigation_brush),
                  uiOutput('trick')
         )
  )
})

output$trick <- renderUI({
  k <- 3
  lapply(1:k,function(i){
    selectInput('foo', label=paste('bar',i), choices=list(i))
  })
})

# example plot
output$plot1 <- renderPlot({

  navParams <- navigationPlotParameters()
  if (is.null(navParams$x))
    return(NULL)

  plot(navParams$x+navParams$x_modifier, navParams$y, col=navParams$colVec, pch=20, cex=1, xaxs='i', yaxs='i', xlim=c(input$navigation_brush$xmin, input$navigation_brush$xmax))
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
