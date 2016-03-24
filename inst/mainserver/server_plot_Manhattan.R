
########################
## FIND OUTLIERS PAGE ##  ------------------------------------------------------------------------------------
########################

#######################################################
## Box: Global Controls for Univariate Distributions ##
#######################################################

# box containing controls for univariate distribution plots
output$box_linearManhattan_controls <- renderUI({
  box(title="Univariate Distributions", status="primary", solidHeader=TRUE, collapsible=TRUE, width=12,
      h3('Header'),
      p('text.'),
      selectizeInput('selectize_linearManhattan_variables', label='Plotting variable(s)', choices=names(cleanData()$y), multiple=TRUE),
      textInput('univariate_main_title',label='Titles',value=NULL,placeholder='Enter main title'),
      textInput('univariate_sub_title',label=NULL,value=NULL,placeholder='Enter subtitle'),
      uiOutput('univariate_lapply_header'),
      uiOutput('univariate_lapply_controls')
  )
})

# header for individual controls
output$univariate_lapply_header <- renderUI({
  k <- ManhattanData()$y_col
  if (k>0) {
    fluidRow(
      column(12,
             hr(),
             h3('Individual Plotting Controls')
      )
    )
  }
})

# individual controls for each univariate plot, produced using lapply method
output$univariate_lapply_controls <- renderUI({
  k <- ManhattanData()$y_col
  if (k>0) {
    lapply(1:k,function(i){
      wellPanel(
        p(strong(input$selectize_linearManhattan_variables[i])),
        checkboxInput(paste('check_log_univariate_',i,sep=''), label='log y-axis', value=FALSE)
      )
    })
  }
})


##########################
## Box: Navigation Plot ##
##########################

# box for navigation plot
output$box_linearManhattan_navigation <- renderUI({
  box(title="Manhattan Plot", status="warning", solidHeader=TRUE, collapsible=FALSE, width=12,
      fluidRow(
        column(6,
               selectInput('restrict_chrom', label='Restrict chromosome', choices=as.list(c('(all)',cleanData()$chromLevels)), selected=navigationPolygons()$chromChosen, width=200)
        )
      ),
      plotOutput('plot_navigation',height=100,width='100%'),
      sliderInput('slider_navigate',
                  label=NULL,
                  min=navigationPolygons()$x_min,
                  max=navigationPolygons()$x_max,
                  value=c(navigationPolygons()$x_min, navigationPolygons()$x_max),
                  step=1,
                  width='100%'),
      div(style="display:inline-block",
          tags$button(id='apply_navigate_button', type="button",
                      class="btn action-button btn-primary",
                      style='font-size:15px; text-align:center',
                      HTML('<i class="icon-star"></i>Apply Changes'))
      )
  ) # end of tabBox
})

# function evaluates when apply_navigate_button is pressed
apply_navigate <- eventReactive(input$apply_navigate_button,{})

# subset cleanData() ready for Manhattan plot based on selectize_linearManhattan_variables
ManhattanData <- reactive({
  data <- cleanData()
  if (is.null(input$selectize_linearManhattan_variables)) {
    output <- list(pos=data$pos,
                   pos_modifier=data$pos_modifier,
                   chrom=data$chrom,
                   chromLevels=data$chromLevels,
                   chromIndex=data$chromIndex,
                   chromMidpoint=data$chromMidpoint,
                   y=NULL,
                   y_col=0,
                   pos_userDefined=data$pos_userDefined,
                   chrom_userDefined=data$chrom_userDefined)
  } else {
    y <- data$y[,input$selectize_linearManhattan_variables,drop=FALSE]
    output <- list(pos=data$pos,
                   pos_modifier=data$pos_modifier,
                   chrom=data$chrom,
                   chromLevels=data$chromLevels,
                   chromIndex=data$chromIndex,
                   chromMidpoint=data$chromMidpoint,
                   y=y,
                   y_col=ncol(y),
                   pos_userDefined=data$pos_userDefined,
                   chrom_userDefined=data$chrom_userDefined)
  }
  return(output)
})

# reactive conductor for subsetting ManhattanData based on restrict_chrom selector, and converting into series of polygons for navigation plot. Returns list(x_min,x_max,y_min,y_max,polygon=list(x,y,chromIndex),chromLevels,chromMidpoint)
navigationPolygons <- reactive({
  nullOutput <- list(x_min=0,x_max=100,y_min=0,y_max=1,polygon=list(),chromLevels=NULL,chromMidpoint=NULL,chromChosen='(all)')
  data <- ManhattanData()

  # if data$y is NULL, return nullOutput
  if (is.null(data$y))
    return(nullOutput)

  # if input$restrict_chrom is NULL, return nullOutput
  chromChosen <- input$restrict_chrom
  if (is.null(chromChosen))
    return(nullOutput)

  # if plotting all chromosomes
  if (chromChosen=='(all)') {
    x <- data$pos + data$pos_modifier
    y <- data$y[,1]
    chromIndex <- data$chromIndex
    chromLevels <- data$chromLevels

  # if plotting single chromosome
  } else {
    x <- subset(data$pos, data$chrom==input$restrict_chrom)
    y <- subset(data$y[,1], data$chrom==input$restrict_chrom)
    chromIndex <- subset(data$chromIndex, data$chrom==input$restrict_chrom)
    chromLevels <- input$restrict_chrom
  }

  # convert x and y to polygons
  polyNum <- 200
  x_min <- min(x,na.rm=TRUE)
  x_max <- max(x,na.rm=TRUE)
  y_min <- min(y,na.rm=TRUE)
  y_max <- max(y,na.rm=TRUE)

  df <- data.frame(y=y, chromIndex=chromIndex)
  breakVec <- seq(x_min, x_max, l=polyNum+1)
  breakDelta <- (breakVec[2]-breakVec[1])/2
  breakMids <- (breakVec[-1]+breakVec[-length(breakVec)])/2
  c <- split(df,f=cut(x,breaks=breakVec))  # split df into list based on which interval x falls into
  output <- list(x_min=x_min,
                 x_max=x_max,
                 y_min=y_min,
                 y_max=y_max,
                 polygon=list(),
                 chromLevels=chromLevels,
                 chromMidpoint=data$chromMidpoint,
                 chromChosen=chromChosen)
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
  if (!is.null(polys$chromLevels)) {
    text(x=polys$chromMidpoint, y=polys$y_max*(0.5+0.7)+polys$y_min*(0.5-0.7), labels=polys$chromLevels, font=2)
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


###########################
## Box: Univariate Plots ##
###########################

# box for plotting univariate distributions
output$box_plot_linearManhattan <- renderUI({
  box(title=NULL, status="warning", solidHeader=FALSE, collapsible=FALSE, width=12,
      h2(input$univariate_main_title, align='center'),
      h3(input$univariate_sub_title, align='center'),
      plotOutput('plot1',height=200*ManhattanData()$y_col+100+20)
  )
})

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

  # work out which plots should have logged axes
  k <- 1#outlierData_sub$y_cols
  axis_log <- NULL
  for (i in 1:k) {
    axis_log <- c(axis_log, eval(parse(text=paste('input$check_log_univariate_',i,sep=''))))
  }

  # produce univariate plots
  mar_top <- 20/(200*k+100+20)
  mar_bot <- 100/(200*k+100+20)
  mid <- 1-mar_top-mar_bot
  for (i in 1:k) {

    # get x and y values
    x <- outlierData_sub$x+outlierData_sub$x_modifier
    y <- outlierData_sub$y[,i]

    # calculate figure position
    plot_top <- mar_bot + mid*(k-i+1)/k
    plot_bot <- mar_bot + mid*(k-i)/k
    par(fig=c(0,1,plot_bot,plot_top),
        mar=c(0,4.1,0,2.1),
        new=i!=1)

    # work out y-axis log action, and plot text error if needed
    log_action <- ''
    if (axis_log[i]) {
      if (any(y<=0,na.rm=TRUE)) {
        plot(0,type='n',xlim=c(0,1),ylim=c(0,1),xaxt='n',yaxt='n',ann=FALSE)
        text(0.5,0.5,labels='(cannot log y-axis due to negative or zero values)',cex=1.5)
        next()
      } else {
        log_action <- 'y'
      }
    }

    # plot empty frame
    plot(x, y, type='n', log=log_action, xaxs='i', xlim=xlim, xaxt=ifelse(i==k,'l','n'))

    #u <- par("usr")
    #polygon(c(u[1],u[2],u[2],u[1]), c(u[3],u[3],u[4],u[4]), col=grey(1))

    # add grid
    tk1 <- axTicks(1)
    tk2 <- axTicks(2)
    abline(v=tk1, h=tk2, col=grey(0.95), lwd=1)

    # finally add points and redraw frame
    points(x, y, col=colVec, pch=20, cex=1)
    box()
  }

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
