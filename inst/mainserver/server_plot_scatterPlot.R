

#######################
## SCATTER PLOT PAGE ##  ------------------------------------------------------------------------------------
#######################


##################
## BOX OF BOXES ##
##################
## (to keep each set of plots+controls in line with each other... )

## Generate K individual BOXES for each univariate plot,
## produced using lapply method, K taken from actionButton:
output$box_scatterPlot <- renderUI({

  k <- 1
  k <- input$new_scatterPlot_button[1] + 1

  if(length(k) > 0){
    if(k > 0){
      lapply(1:k,function(i){
        # .get.scatterPlot.controls(cleanData())

        dat <- title.k <- NULL

        ## get title
        title.k <- paste("Scatter Plot #", k, sep = " ")

        ## get data
        dat <- cleanData()

        ## get box of boxes
        if(!is.null(dat)){
          box(title=title.k,
              status="warning",
              solidHeader=TRUE,
              collapsible=TRUE,
              width=12,

              fluidRow(
                column(4,
                       .get.scatterPlot.controls(dat)
                ),

                column(8,
                       .get.scatterPlot.plot(dat),
                       .get.scatterPlot.controls.aes(dat)
                )
              )
          )
        }
      })
    }
  }
})


#######################################################
## Box: Global Controls for Univariate Distributions ##
#######################################################

## Fn to generate boxes containing controls for univariate distribution plots

.get.scatterPlot.controls <- function(dat){

  out <- NULL
  x.var.choices <- x.var.sel <-
    y.var.choices <- y.var.sel <-
    o.var.choices <- o.var.sel <- NULL

  if(!is.null(dat)){

    ## get numeric variables
    numCols <- which(sapply(c(1:ncol(dat$y)),
                            function(e) is.numeric(dat$y[,e])))

    var.choices <- c("Position", "Chromosome", names(dat$y)[numCols])

    x.var.choices <- y.var.choices <- o.var.choices <- var.choices

    x.var.sel <- x.var.choices[3]
    y.var.sel <- y.var.choices[4]

    o.var.sel <- x.var.choices[5]
  }

  out <-
    box(title="Select Variables:", # "Univariate Distributions"
        status="primary",
        solidHeader=TRUE,
        collapsible=TRUE,
        width=12,

        ###################
        ## Choose x-axis ##
        ###################

        box(title="Select x-axis:", # "Univariate Distributions"
            status="info",
            #status = "primary",
            solidHeader=TRUE,
            collapsible=TRUE,
            width=12,

            ## NOTE: Would like to be able to pull the Chromosome and Position variables
            ## selected/generated in the Format Data tab to be available as options
            ## and autoatically selected below...

            ## Choose x-axis variable
            selectizeInput('scatterPlot_xaxis',
                           label = 'X-axis:',
                           choices = x.var.choices,
                           selected = x.var.sel,
                           multiple = FALSE),

            ## log(x-axis) ?
            radioButtons("scatterPlot_logx",
                         label = "Log x-axis?",
                         choices = list("log2", "log10", "none"),
                         selected="none",
                         inline=TRUE),

            ## Flip x-axis ?
            radioButtons("scatterPlot_flipx",
                         label = "Invert x-axis?",
                         choices = list("Yes", "No"),
                         selected="No",
                         inline=TRUE)
        ),

        ###################
        ## Choose y-axis ##
        ###################

        box(title="Select y-axis:",
            status="info",
            #status = "primary",
            solidHeader=TRUE,
            collapsible=TRUE,
            width=12,


            ## Choose y-axis variable
            selectizeInput('scatterPlot_yaxis',
                           label = 'Y-axis:',
                           choices = y.var.choices,
                           selected = y.var.sel,
                           multiple = FALSE),

            ## log(y-axis) ?
            radioButtons("scatterPlot_logy",
                         label = "Log y-axis?",
                         choices = list("log2", "log10", "none"),
                         selected="none",
                         inline=TRUE),

            ## Flip y-axis ?
            radioButtons("scatterPlot_flipy",
                         label = "Invert y-axis?",
                         choices = list("Yes", "No"),
                         selected="No",
                         inline=TRUE)

        ),


        ###############################################
        ## Choose outlier variable (usually p-value) ##
        ###############################################

        ## NOTE: I'm not 100% sure what the best way to refer to this variable is...
        ## ie. "Second variable" or "Outlier detection variable" or "Univariate outlier detection variable"??

        box(title="Select outlier variable:",
            status="info",
            #status = "primary",
            solidHeader=TRUE,
            collapsible=TRUE,
            width=12,

            ## Mark outliers by second variable (usually p-value)
            h5(strong('Highlight outliers by this variable:')),
            p("For example, you may wish to identify outliers according to a p-value
              that is recorded in another column of the data table."),
            selectizeInput('scatterPlot_outlier.var',
                           label = NULL,
                           choices = o.var.choices,
                           selected = o.var.sel,
                           multiple = FALSE),

            ## Cut-off for outliers to overlay
            # eg 0.01
            textInput("scatterPlot_outlier.cutoff",
                      label = "Cut-off for outliers to overlay",
                      value = 0.05),

            radioButtons("scatterPlot_outlier.tail",
                         label = "Tail",
                         choices = c("Lower", "Upper"),
                         selected = "Lower",
                         inline=TRUE)

            )

        )

  return(out)
} # end .get.scatterPlot.controls



## Generate K individual controls for each univariate plot,
## produced using lapply method, K taken from actionButton:
# output$box_scatterPlot_controls <- renderUI({
#   k <- 1
#   k <- input$new_scatterPlot_button[1] + 1
#   if(length(k) > 0){
#     if(k > 0) {
#       lapply(1:k,function(i){
#         .get.scatterPlot.controls(cleanData())
#       })
#     }
#   }
# })




###################################
## .get.scatterPlot.controls.aes ##
###################################
## fn to get widgets to control plot AESTHETICS under plot
.get.scatterPlot.controls.aes <- function(dat){

  out <- NULL

  out <-
    box(title="Adjust Plot Aesthetics:",
        status="warning",
        solidHeader=TRUE,
        collapsible=TRUE,
        width=12,

    h4("Scatter aesthetics:"),

    ## selectInput w col.pals
    selectizeInput("scatterPlot_col.pal",
                  label="Colour palette",
                  choices = list("Heat colours" = "heat.colors",
                                 "Terrain colours" = "terrain.colors",
                                 "Topo colours" = "topo.colors",
                                 "CM colours" = "cm.colors",
                                 "Gray colours" = "gray.colors"),
                  selected = "heat.colors",
                  multiple=FALSE),


    sliderInput("scatterPlot_n.bins",
                label = "Number of bins:",
                min = 2, max = 1000,
                value = 100,
                step = 1),

    hr(),

    h4("Outlier aesthetics:"),

    selectizeInput("scatterPlot_outlier.col.bg",
                   label = "Outlier colour (fill):",
                   choices = list("Red" = "red",
                                  "Orange" = "orange",
                                  "Yellow" = "yellow",
                                  "Green" = "green",
                                  "Blue" = "blue",
                                  "Purple" = "purple"),
                   selected = "purple",
                   multiple=FALSE),

    selectizeInput("scatterPlot_outlier.col",
                  label = "Outlier colour (outline):",
                  choices = list("Red" = "red",
                                 "Orange" = "orange",
                                 "Yellow" = "yellow",
                                 "Green" = "green",
                                 "Blue" = "blue",
                                 "Purple" = "purple"),
                  selected = "blue",
                  multiple=FALSE),

    selectizeInput("scatterPlot_outlier.pch",
                   label = "Outlier shape:",
                   choices = list("Circle" = "21",
                                  "Square" = "22",
                                  "Diamond" = "23",
                                  "Triangle, point-up" = "24",
                                  "Triangle, point-down" = "25"
                                  ),
                   selected = "24",
                   multiple=FALSE),

    sliderInput("scatterPlot_outlier.transp",
                label = "Outlier transparency:",
                min = 0, max = 1,
                value = 0.25,
                step = 0.05),

    sliderInput("scatterPlot_outlier.cex",
                label = "Outlier size:",
                min = 0, max = 3,
                value = 1.5,
                step = 0.1)

    ) # end box

  return(out)

} # end .get.scatterPlot.controls.aes


## Generate K individual controls for each univariate plot,
## produced using lapply method, K taken from actionButton:
# output$box_scatterPlot_controls_aes <- renderUI({
#   k <- 1
#   k <- input$new_scatterPlot_button[1] + 1
#   if(length(k) > 0){
#     if(k > 0) {
#       lapply(1:k,function(i){
#         .get.scatterPlot.controls.aes(cleanData())
#       })
#     }
#   }
# })


####################################
## BUTTON: Generate another plot? ##
####################################
output$box_scatterPlot_button <- renderUI({
  box(
    title = "Generate another plot?",
    solidHeader = TRUE,
    status = "primary",
    value = NULL,
    width=12,

    ## button
    actionButton(inputId = "new_scatterPlot_button",
                 label = "Yes, please!",
                 icon = icon("cog"))
  )
})

# output$buttonTest_scatterPlot <- renderPrint({
#   print(input$new_scatterPlot_button[1])
# })



###########################
## Box: scatterPlot Plot ##
###########################

# output$box_scatterPlot_plot <- renderUI({
#   box(title="Scatter Plot",
#       status="warning",
#       solidHeader=TRUE,
#       collapsible=TRUE,
#       width=12,
#
#       # h3("ScatterPlot"),
#
#       plotOutput("plot_scatterPlot_plot")
#   )
# })

.get.scatterPlot.plot <- function(dat){
  out <- NULL
  out <-
    box(title=NULL,
        status="warning",
        solidHeader=FALSE,
        collapsible=TRUE,
        width=12,
        plotOutput("plot_scatterPlot_plot")
    )
  return(out)
}


# output$foo <- renderText({
#   dat <- var <- toPlot <- NULL
#   dat <- cleanData()
#   var <- input$scatterPlot_var
# })


############################
## Plot: scatterPlot_plot ##
############################

## set n.bins reactiveValue:
# rv <- reactiveValues()
# rv$n.bins <- 100 # set to initial selected value of textInput n.bins


## get hist plot output:
output$plot_scatterPlot_plot <- renderPlot({

  out <- xSelection <- ySelection <- NULL

  ## Get x-axis & y-axis
  xSelection <- input$scatterPlot_xaxis
  ySelection <- input$scatterPlot_yaxis

  ## Get scatter plot
  out <- .get.scatterPlot(input)

  ## PLOT SCATTER PLOT
  if(!is.null(out)){
    if(!is.null(xSelection) & !is.null(ySelection)) {
      # if(!is.null(rv$n.bins)){
        out
      # }
    }
  }

})




######################
## .get.scatterPlot ##
######################
.get.scatterPlot <- function(input){

  scatterplot <- dat <- xData <- yData <- NULL

  ## Get x-axis & y-axis
  xSelection <- input$scatterPlot_xaxis
  ySelection <- input$scatterPlot_yaxis

  ## Log x-axis?
  logx <- input$scatterPlot_logx
  #print(logx)
  if(logx=="none"){logx=NULL}else{
    if(sum(xSelection<0)>0){print("Error: You are trying to log-transform
                                  negative values in the X variable.
                                  These values will not be plotted.")}
    if(logx=="log2"){logx=2}
    if(logx=="log10"){logx=10}
  }

  ## Log y-axis?
  logy <- input$scatterPlot_logy
  if(logy=="none"){logy=NULL}else{
    if(sum(ySelection<0)>0){print("Error: You are trying to log-transform
                                  negative values in the Y variable.
                                  These values will not be plotted.")}
    if(logy=="log2"){logy=2}
    if(logy=="log10"){logy=10}
  }

  ## Invert x-axis?
  flipX <- input$scatterPlot_flipx
  if(flipX=="No"){flipX=1}else{
    if(flipX=="Yes"){flipX=-1}}

  ## Invert y-axis?
  flipY <- input$scatterPlot_flipy
  if(flipY=="No"){flipY=1}else{
    if(flipY=="Yes"){flipY=-1}}

  #########################
  ## Get plot aesthetics ##
  #########################

  ## Get plot aesthetics
  col.pal <- input$scatterPlot_col.pal
  n <- 100
  start <- 0.25
  end <- 0.9
  alpha <- 1
  if(col.pal == "gray.colors"){
    col.pal <- eval(parse(text=paste(col.pal, "(n=n, start=start, end=end)", sep="")))
  }else{
    col.pal <- eval(parse(text=paste(col.pal, "(n=n, alpha=alpha)", sep="")))
  }

  outlier.col.bg <- input$scatterPlot_outlier.col.bg
  outlier.col <- input$scatterPlot_outlier.col
  outlier.transp <- input$scatterPlot_outlier.transp

  outlier.pch <- as.numeric(input$scatterPlot_outlier.pch)
  outlier.cex <- as.numeric(input$scatterPlot_outlier.cex)

  n.bins <- as.numeric(input$scatterPlot_n.bins)


  ## Get outlier var
  outlier.var <- input$scatterPlot_outlier.var #this is the 3rd variable
  cutoff <- as.numeric(input$scatterPlot_outlier.cutoff)
  tail <- input$scatterPlot_outlier.tail

  ## Get data and plot output
  if(!is.null(cleanData())){
    if(!is.null(xSelection) && !is.null(ySelection)){

      ## Get data
      dat <- cleanData()


      ## Get X & Y variables

      # xData = dat[,names(dat)==xSelection]
      # yData = dat[,names(dat)==ySelection]

      ## Get x-variable data
      if(!is.null(xSelection)){
        ## Get variable to plot
        if(xSelection == "Position"){
          ## could be used to check for missing values...
          xData <- eval(parse(text="dat$pos"))
        }else{
          if(xSelection == "Chromosome"){
            ## could be used to check for representation/length of each chromosome
            xData <- eval(parse(text="dat$chrom"))
          }else{
            xData <- eval(parse(text=paste("dat$y", xSelection, sep="$")))
          }
        }
      }

      ## Get y-variable data
      if(!is.null(ySelection)){
        ## Get variable to plot
        if(ySelection == "Position"){
          ## could be used to check for missing values...
          yData <- eval(parse(text="dat$pos"))
        }else{
          if(ySelection == "Chromosome"){
            ## could be used to check for representation/length of each chromosome
            yData <- eval(parse(text="dat$chrom"))
          }else{
            yData <- eval(parse(text=paste("dat$y", ySelection, sep="$")))
          }
        }
      }


      ## Get outlier-variable data
      # outlier.Data = dat[,names(dat)==outlier.var]

      if(!is.null(outlier.var)){
        ## Get variable to plot
        if(outlier.var == "Position"){
          ## could be used to check for missing values...
          outlier.Data <- eval(parse(text="dat$pos"))
        }else{
          if(outlier.var == "Chromosome"){
            ## could be used to check for representation/length of each chromosome
            outlier.Data <- eval(parse(text="dat$chrom"))
          }else{
            outlier.Data <- eval(parse(text=paste("dat$y", outlier.var, sep="$")))
          }
        }
      }

      if(length(logx)==1){xData=log(xData+1e-40, logx)}
      if(length(logy)==1){yData=log(yData+1e-40, logy)}

      # print(c(1,cutoff))
      if(is.na(cutoff)){cutoff=0.01}
      if(tail=="Upper"){
        cutoff=(1-cutoff)
      }
      # print(c(2,cutoff))
      outlier.DataNoNA <- outlier.Data[!is.na(outlier.Data)]
      outlier.DataNew <- rank(outlier.DataNoNA)/length(outlier.DataNoNA)
      outlier.DataNew2 <- outlier.Data
      outlier.DataNew2[!is.na(outlier.Data)] <- outlier.DataNew

      if(tail=="Lower"){
        xData_sub <- xData[outlier.DataNew2<=cutoff]
        yData_sub <- yData[outlier.DataNew2<=cutoff]
      }
      if(tail=="Upper"){
        xData_sub <- xData[outlier.DataNew2>=cutoff]
        yData_sub <- yData[outlier.DataNew2>=cutoff]
      }

      xData <- xData*flipX
      yData <- yData*flipY
      xData_sub <- xData_sub*flipX
      yData_sub <- yData_sub*flipY
      #print(cbind(xData_sub,yData_sub))
      #print(logy)
      #print(c(min(yData,na.rm=TRUE), max(yData, na.rm=TRUE)))

      # get colors
      #          get.levels <- levels(as.factor(colData))
      #          n.levels <- length(get.levels)
      #          colIndex <- as.numeric(as.factor(colData))
      #           if(!(colPal=="black")){
      #            myCol <- get(colPal)(n.levels)[colIndex]
      #            }else(myCol <- rgb(0,0,0,0.2))

      # produce plot
      #scatterplot <- plot(xData, yData, xlab=xSelection, ylab=ySelection, col=myCol, pch=20)
      scatterplot <- .plot_2D(xData, yData, xlab=xSelection, ylab=ySelection,
                              n.bins=n.bins, x_sub=xData_sub, y_sub=yData_sub,
                              col.pal=col.pal,
                              outlier.col=outlier.col, outlier.col.bg=outlier.col.bg,
                              outlier.transp=outlier.transp,
                              outlier.pch=outlier.pch, outlier.cex=outlier.cex)

    }
  }
  return(scatterplot)
} # end .get.scatterPlot



##############
## .plot_2D ##
##############

### Plot function, need to move to function area

## QUESTION: DO WE WANT TO RELEASE plot_2D FOR
## PUBLIC USE AS A STAND-ALONE FUNCTION????????????????????????????????????????????????????
## (If so, we need to (1) remove the (.) from
## the front of the fn name and in every instance of its use in the app,
## and (2) document the fn using roxygen2-style comments
## s.t it is added to the namespace and Rd/man folder etc.)

.plot_2D<- function(x, y,
                    xlab, ylab,
                    xlim=NULL, ylim=NULL,
                    n.bins,
                    x_sub, y_sub,
                    col.pal,
                    outlier.col, outlier.col.bg,
                    outlier.transp,
                    outlier.pch, outlier.cex){

  require(ash)
  require(fields)
  require(adegenet)

  if(outlier.transp != 0){
    outlier.transp <- 1 - outlier.transp
    outlier.col <- transp(outlier.col, alpha = outlier.transp)
    outlier.col.bg <- transp(outlier.col.bg, alpha = outlier.transp)
  }

  data1 <- cbind(x, y)
  data1b <- data1[complete.cases(data1),]

  if(length(xlim)==0){
    xlim_up <- max(x, na.rm=TRUE)
    xlim_lower <- min(x, na.rm=TRUE)
  }

  if(length(ylim)==0){
    ylim_up <- max(y, na.rm=TRUE)
    ylim_lower <- min(y, na.rm=TRUE)
  }

  binned <- bin2(data1b,
                 matrix(c(xlim_lower,xlim_up,ylim_lower,ylim_up), 2,2, byrow=TRUE),
                 nbin=c(n.bins,n.bins))
  binned$nc[binned$nc==0]=NA

  ## SCATTER PLOT
  image.plot(seq(xlim_lower,xlim_up,length.out = n.bins),
             seq(ylim_lower,ylim_up, length.out=n.bins),
             binned$nc,
             xlab=xlab, ylab=ylab, add=FALSE, col=col.pal)

  ## ADD OUTLIER POINTS
  points(x_sub, y_sub, pch=outlier.pch, cex=outlier.cex, col=outlier.col, bg=outlier.col.bg)

  ## SET TITLE TO VALUE BEING PLOTTED
  ## NOTE: to be changed to textInput( w x- and ySelection selected)!!!!!!
  if(!is.null(xlab) & !is.null(ylab)) title(paste(xlab, "vs.", ylab, sep=" "))
    # title(paste("xlab", eval(parse(text= HTML(em("versus")))), "ylab", sep=" "))
} # end .plot_2D





