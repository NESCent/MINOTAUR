

#######################
## SCATTER PLOT PAGE ##  ------------------------------------------------------------------------------------
#######################


## generate reactiveValues lists for all initial values

## variables
rv_scatterPlot_button <- shiny::reactiveValues()
rv_scatterPlot_button <- 1 # 0
rv_scatterPlot_xaxis <- shiny::reactiveValues()
rv_scatterPlot_yaxis <- shiny::reactiveValues()
rv_scatterPlot_logx <- shiny::reactiveValues()
rv_scatterPlot_logy <- shiny::reactiveValues()
rv_scatterPlot_flipx <- shiny::reactiveValues()
rv_scatterPlot_flipy <- shiny::reactiveValues()

rv_scatterPlot_outlier.var <- shiny::reactiveValues()
rv_scatterPlot_outlier.cutoff <- shiny::reactiveValues()
rv_scatterPlot_outlier.tail <- shiny::reactiveValues()

## aesthetics
rv_scatterPlot_col.pal <- shiny::reactiveValues()
rv_scatterPlot_n.bins <- shiny::reactiveValues()
rv_scatterPlot_grid <- shiny::reactiveValues()

rv_scatterPlot_outlier.col.bg <- shiny::reactiveValues()
rv_scatterPlot_outlier.col <- shiny::reactiveValues()
rv_scatterPlot_outlier.pch <- shiny::reactiveValues()
rv_scatterPlot_outlier.transp <- shiny::reactiveValues()
rv_scatterPlot_outlier.cex <- shiny::reactiveValues()






#####################################
## .set.reactiveValues.scatterPlot ##
#####################################
## fn to set reactiveValues initially for each k:
.set.reactiveValues.scatterPlot <- function(dat, k){

  k <- as.character(k)

  x.var.choices <- x.var.sel <-
    y.var.choices <- y.var.sel <-
    o.var.choices <- o.var.sel <- NULL

  ## get variables
  if(!is.null(dat)){

    ## get numeric variables
    numCols <- which(sapply(c(1:ncol(dat$y)),
                            function(e) is.numeric(dat$y[,e])))

    var.choices <- c("Position", "Chromosome", names(dat$y)[numCols])

    x.var.choices <- y.var.choices <- o.var.choices <- var.choices

    x.var.sel <- x.var.choices[3]
    y.var.sel <- y.var.choices[4]

    o.var.sel <- o.var.choices[5]

    ## set intial values
    rv_scatterPlot_xaxis[[k]] <- x.var.sel
    rv_scatterPlot_yaxis[[k]] <- y.var.sel
    rv_scatterPlot_logx[[k]] <- "none"
    rv_scatterPlot_logy[[k]] <- "none"
    rv_scatterPlot_flipx[[k]] <- "No"
    rv_scatterPlot_flipy[[k]] <- "No"
    rv_scatterPlot_outlier.var[[k]] <- o.var.sel
    rv_scatterPlot_outlier.cutoff[[k]] <- 0.05
    rv_scatterPlot_outlier.tail[[k]] <- "Lower"

    rv_scatterPlot_col.pal[[k]] <- "heat.colors"
    rv_scatterPlot_n.bins[[k]] <- 100
    rv_scatterPlot_grid[[k]] <- FALSE

    rv_scatterPlot_outlier.col.bg[[k]] <- "purple"
    rv_scatterPlot_outlier.col[[k]] <- "blue"
    rv_scatterPlot_outlier.pch[[k]] <- "24"
    rv_scatterPlot_outlier.transp[[k]] <- 0.25
    rv_scatterPlot_outlier.cex[[k]] <- 1.5

  }
} # end .set.reactiveValues.scatterPlot



########################################
## .update.reactiveValues.scatterPlot ##
########################################
## fn to set reactiveValues initially for each k:
.update.reactiveValues.scatterPlot <- function(dat, k){

  k <- as.character(k)

  x.var.choices <- x.var.sel <-
    y.var.choices <- y.var.sel <-
    o.var.choices <- o.var.sel <- NULL

  ## get variables
  if(!is.null(dat)){

    ## Get currently-selected values:

    ## Get x-axis & y-axis
    xSelection <- eval(parse(text=paste("input$scatterPlot_xaxis", k, sep="_")))
    ySelection <- eval(parse(text=paste("input$scatterPlot_yaxis", k, sep="_")))

    logx <- eval(parse(text=paste("input$scatterPlot_logx", k, sep="_")))
    logy <- eval(parse(text=paste("input$scatterPlot_logy", k, sep="_")))
    flipX <- eval(parse(text=paste("input$scatterPlot_flipx", k, sep="_")))
    flipY <- eval(parse(text=paste("input$scatterPlot_flipy", k, sep="_")))

    ## Get plot aesthetics
    col.pal <- eval(parse(text=paste("input$scatterPlot_col.pal", k, sep="_")))
    n.bins <- eval(parse(text=paste("input$scatterPlot_n.bins", k, sep="_")))
    grid <- eval(parse(text=paste("input$scatterPlot_grid", k, sep="_")))

    outlier.col.bg <- eval(parse(text=paste("input$scatterPlot_outlier.col.bg", k, sep="_")))
    outlier.col <- eval(parse(text=paste("input$scatterPlot_outlier.col", k, sep="_")))
    outlier.transp <- eval(parse(text=paste("input$scatterPlot_outlier.transp", k, sep="_")))
    outlier.pch <- eval(parse(text=paste("input$scatterPlot_outlier.pch", k, sep="_")))
    outlier.cex <- eval(parse(text=paste("input$scatterPlot_outlier.cex", k, sep="_")))



    ## Get outlier var
    outlier.var <- eval(parse(text=paste("input$scatterPlot_outlier.var", k, sep="_")))
    cutoff <- eval(parse(text=paste("input$scatterPlot_outlier.cutoff", k, sep="_")))
    tail <- eval(parse(text=paste("input$scatterPlot_outlier.tail", k, sep="_")))

    ## update "intial" values to current values
    rv_scatterPlot_xaxis[[k]] <- xSelection
    rv_scatterPlot_yaxis[[k]] <- ySelection
    rv_scatterPlot_logx[[k]] <- logx
    rv_scatterPlot_logy[[k]] <- logy
    rv_scatterPlot_flipx[[k]] <- flipX
    rv_scatterPlot_flipy[[k]] <- flipY
    rv_scatterPlot_outlier.var[[k]] <- outlier.var
    rv_scatterPlot_outlier.cutoff[[k]] <- cutoff
    rv_scatterPlot_outlier.tail[[k]] <- tail

    rv_scatterPlot_n.bins[[k]] <- n.bins
    rv_scatterPlot_col.pal[[k]] <- col.pal
    rv_scatterPlot_grid[[k]] <- grid

    rv_scatterPlot_outlier.col.bg[[k]] <- outlier.col.bg
    rv_scatterPlot_outlier.col[[k]] <- outlier.col
    rv_scatterPlot_outlier.pch[[k]] <- outlier.pch
    rv_scatterPlot_outlier.transp[[k]] <- outlier.transp
    rv_scatterPlot_outlier.cex[[k]] <- outlier.cex

  }
} # end .update.reactiveValues.scatterPlot



## update K & set reactiveValues[[k]] if button pressed
observe({

  k <- input$new_scatterPlot_button

  if(length(k) == 1){
    k <- k[1]+1
    ## if input button updates, set new panel of initial input values

    dat <- data_outliers()

    ## if K updates:
    if(!is.null(dat)){

    if(k == 1){
      .set.reactiveValues.scatterPlot(dat, k)
    }else{
    if(k > rv_scatterPlot_button){
      ## update rv_scatterPlot_button
      rv_scatterPlot_button <- k

        # set reactive values for Kth element of rv lists
        .set.reactiveValues.scatterPlot(dat, k)
        # .update.reactiveValues.scatterPlot(dat, k)

        ## if more than one panel requested, update "initial" values for plots 1:k-1
        if(k > 1){
          for(i in 1:(k-1)){
            .update.reactiveValues.scatterPlot(dat, i)
          }
        }
    }
    }
    }
  }
})



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

        dat <- title.k <- NULL

        ## get title
        title.k <- paste("Scatter Plot #", i, sep = " ")

        ## get data
        dat <- data_outliers()

        ## get box of boxes
        if(!is.null(dat)){
          box(title=title.k,
              status="warning",
              solidHeader=TRUE,
              collapsible=TRUE,
              width=12,

              fluidRow(
                column(4,
                       .get.scatterPlot.controls(dat, i)
                ),

                column(8,
                       .get.scatterPlot.plot(dat, i),
                       .get.scatterPlot.controls.aes(dat, i)
                )
              ),

              style = list('background-color: #B6B6B6') # dark gray (amber)
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

.get.scatterPlot.controls <- function(dat, k=1){

  k <- as.character(k)

  out <- NULL

  ## get variables
  if(!is.null(dat)){

    ## get numeric variables
    numCols <- which(sapply(c(1:ncol(dat$y)),
                            function(e) is.numeric(dat$y[,e])))

    var.choices <- c("Position", "Chromosome", names(dat$y)[numCols])

    x.var.choices <- y.var.choices <- o.var.choices <- var.choices

  }

  ## get id's | k
  # k <- 1
  id_scatterPlot_xaxis <- paste("scatterPlot_xaxis", k, sep="_")
  id_scatterPlot_logx <- paste("scatterPlot_logx", k, sep="_")
  id_scatterPlot_flipx <- paste("scatterPlot_flipx", k, sep="_")
  id_scatterPlot_yaxis <- paste("scatterPlot_yaxis", k, sep="_")
  id_scatterPlot_logy <- paste("scatterPlot_logy", k, sep="_")
  id_scatterPlot_flipy <- paste("scatterPlot_flipy", k, sep="_")
  id_scatterPlot_outlier.var <- paste("scatterPlot_outlier.var", k, sep="_")
  id_scatterPlot_outlier.cutoff <- paste("scatterPlot_outlier.cutoff", k, sep="_")
  id_scatterPlot_outlier.tail <- paste("scatterPlot_outlier.tail", k, sep="_")

  out <-
    box(title="Select Variables:", # "Univariate Distributions"
        # status="primary",
        status="warning",
        solidHeader=FALSE,
        collapsible=TRUE,
        width=12,

        ###################
        ## Choose x-axis ##
        ###################

        box(title="Select x-axis:", # "Univariate Distributions"
            # status="info",
            # status = "primary",
            status="warning",
            solidHeader=TRUE,
            collapsible=TRUE,
            width=12,

            ## NOTE: Would like to be able to pull the Chromosome and Position variables
            ## selected/generated in the Format Data tab to be available as options
            ## and autoatically selected below...

            ## Choose x-axis variable
            selectizeInput(id_scatterPlot_xaxis,
                           label = 'X-axis:',
                           choices = x.var.choices,
                           selected = rv_scatterPlot_xaxis[[k]], # x.var.sel,
                           multiple = FALSE),

            ## log(x-axis) ?
            radioButtons(id_scatterPlot_logx,
                         label = "Log x-axis?",
                         choices = list("log2", "log10", "none"),
                         selected= rv_scatterPlot_logx[[k]], # "none",
                         inline=TRUE),

            ## Flip x-axis ?
            radioButtons(id_scatterPlot_flipx,
                         label = "Invert x-axis?",
                         choices = list("Yes", "No"),
                         selected= rv_scatterPlot_flipx[[k]], # "No",
                         inline=TRUE),

            style = list('background-color: #FFECB3') # pale amber
        ),

        ###################
        ## Choose y-axis ##
        ###################

        box(title="Select y-axis:",
            # status="info",
            # status = "primary",
            status="warning",
            solidHeader=TRUE,
            collapsible=TRUE,
            width=12,

            ## Choose y-axis variable
            selectizeInput(id_scatterPlot_yaxis,
                           label = 'Y-axis:',
                           choices = y.var.choices,
                           selected =  rv_scatterPlot_yaxis[[k]], # y.var.sel,
                           multiple = FALSE),

            ## log(y-axis) ?
            radioButtons(id_scatterPlot_logy,
                         label = "Log y-axis?",
                         choices = list("log2", "log10", "none"),
                         selected= rv_scatterPlot_logy[[k]], # "none",
                         inline=TRUE),

            ## Flip y-axis ?
            radioButtons(id_scatterPlot_flipy,
                         label = "Invert y-axis?",
                         choices = list("Yes", "No"),
                         selected= rv_scatterPlot_flipy[[k]], # "No",
                         inline=TRUE),

            style = list('background-color: #FFECB3') # pale amber

        ),


        ###############################################
        ## Choose outlier variable (usually p-value) ##
        ###############################################

        ## NOTE: I'm not 100% sure what the best way to refer to this variable is...
        ## ie. "Second variable" or "Outlier detection variable" or "Univariate outlier detection variable"??

        box(title="Select outlier variable:",
            # status="info",
            # status = "primary",
            status="warning",
            solidHeader=TRUE,
            collapsible=TRUE,
            width=12,

            ## Mark outliers by second variable (usually p-value)
            h5(strong('Highlight outliers by this variable:')),
            p("For example, you may wish to identify outliers according to a p-value
              that is recorded in another column of the data table."),
            selectizeInput(id_scatterPlot_outlier.var,
                           label = NULL,
                           choices = o.var.choices,
                           selected = rv_scatterPlot_outlier.var[[k]], # o.var.sel,
                           multiple = FALSE),

            ## Cut-off for outliers to overlay
            # eg 0.01
            textInput(id_scatterPlot_outlier.cutoff,
                      label = "Cut-off for outliers to overlay",
                      value =  rv_scatterPlot_outlier.cutoff[[k]] # 0.05
                      ),

            radioButtons(id_scatterPlot_outlier.tail,
                         label = "Tail",
                         choices = c("Lower", "Upper", "Two-tailed"),
                         selected =  rv_scatterPlot_outlier.tail[[k]], # "Lower",
                         inline=TRUE),

            style = list('background-color: #FFECB3') # pale amber
            )

        )

  return(out)
} # end .get.scatterPlot.controls





###################################
## .get.scatterPlot.controls.aes ##
###################################
## fn to get widgets to control plot AESTHETICS under plot
.get.scatterPlot.controls.aes <- function(dat, k=1){

  k <- as.character(k)

  ## get Id's | k
  id_scatterPlot_col.pal <- paste("scatterPlot_col.pal", k, sep="_")
  id_scatterPlot_n.bins <- paste("scatterPlot_n.bins", k, sep="_")
  id_scatterPlot_grid <- paste("scatterPlot_grid", k, sep="_")
  id_scatterPlot_outlier.col.bg <- paste("scatterPlot_outlier.col.bg", k, sep="_")
  id_scatterPlot_outlier.col <- paste("scatterPlot_outlier.col", k, sep="_")
  id_scatterPlot_outlier.pch <- paste("scatterPlot_outlier.pch", k, sep="_")
  id_scatterPlot_outlier.transp <- paste("scatterPlot_outlier.transp", k, sep="_")
  id_scatterPlot_outlier.cex <- paste("scatterPlot_outlier.cex", k, sep="_")


  out <- NULL

  out <-
    box(title="Adjust Plot Aesthetics:",
        status="warning",
        solidHeader=FALSE,
        collapsible=TRUE,
        width=12,

    # h4("Scatter aesthetics:"),
    box(title="Scatter aesthetics:",
        status="warning",
        solidHeader=TRUE,
        collapsible=TRUE,
        width=12,

        fluidRow(column(4,
    ## selectInput w col.pals
    selectizeInput(id_scatterPlot_col.pal,
                  label="Colour palette:",
                  choices = list("Heat colours" = "heat.colors",
                                 "Terrain colours" = "terrain.colors",
                                 "Topo colours" = "topo.colors",
                                 "CM colours" = "cm.colors",
                                 "Gray colours" = "gray.colors"),
                  selected =  rv_scatterPlot_col.pal[[k]], # "heat.colors",
                  multiple=FALSE)),

    column(4,
    sliderInput(id_scatterPlot_n.bins,
                label = "Number of bins:",
                min = 2, max = 1000,
                value =  rv_scatterPlot_n.bins[[k]], # 100,
                step = 1)),


    column(4,
    radioButtons(id_scatterPlot_grid,
                 label="Overlay grid?",
                 choices=list("Yes" = TRUE,
                              "No" = FALSE),
                 selected = rv_scatterPlot_grid[[k]],
                 inline = TRUE)
    )),

    style = list('background-color: #FFECB3') # pale amber
    ),


    box(title="Outlier aesthetics:",
        status="warning",
        solidHeader=TRUE,
        collapsible=TRUE,
        width=12,

        fluidRow(
          column(4,
    selectizeInput(id_scatterPlot_outlier.col.bg,
                   label = "Outlier colour (fill):",
                   choices = list("Red" = "red",
                                  "Orange" = "orange",
                                  "Yellow" = "yellow",
                                  "Green" = "green",
                                  "Blue" = "blue",
                                  "Purple" = "purple"),
                   selected =  rv_scatterPlot_outlier.col.bg[[k]], # "purple",
                   multiple=FALSE)),

    column(4,
    selectizeInput(id_scatterPlot_outlier.col,
                  label = "Outlier colour (outline):",
                  choices = list("Red" = "red",
                                 "Orange" = "orange",
                                 "Yellow" = "yellow",
                                 "Green" = "green",
                                 "Blue" = "blue",
                                 "Purple" = "purple"),
                  selected =  rv_scatterPlot_outlier.col[[k]], # "blue",
                  multiple=FALSE)),

    column(4,
    selectizeInput(id_scatterPlot_outlier.pch,
                   label = "Outlier shape:",
                   choices = list("Circle" = "21",
                                  "Square" = "22",
                                  "Diamond" = "23",
                                  "Triangle, point-up" = "24",
                                  "Triangle, point-down" = "25"
                                  ),
                   selected =  rv_scatterPlot_outlier.pch[[k]], # "24",
                   multiple=FALSE))
    ),

    hr(),

    fluidRow(
      column(6,
    sliderInput(id_scatterPlot_outlier.transp,
                label = "Outlier transparency:",
                min = 0, max = 1,
                value =  rv_scatterPlot_outlier.transp[[k]], # 0.25,
                step = 0.05)),

    column(6,
    sliderInput(id_scatterPlot_outlier.cex,
                label = "Outlier size:",
                min = 0, max = 3,
                value =  rv_scatterPlot_outlier.cex[[k]], # 1.5,
                step = 0.1))
    ),

    style = list('background-color: #FFECB3') # pale amber
    )

    ) # end box

  return(out)

} # end .get.scatterPlot.controls.aes



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




##########################
## get.scatterPlot.plot ##
##########################
.get.scatterPlot.plot <- function(dat, k=1){

  out <- NULL

  if(!is.null(k)){

    ## get unique outputId
    id_scatterPlot <- paste("id_scatterPlot", k, sep="_")

  out <-
    box(title=NULL,
        status="warning",
        solidHeader=FALSE,
        collapsible=TRUE,
        width=12,
        # plotOutput("plot_scatterPlot_plot")
        renderPlot(plotOutput(
          outputId = id_scatterPlot,
          .get.scatterPlot(input, k=k)))
    )
  }
  return(out)
}









######################
## .get.scatterPlot ##
######################
.get.scatterPlot <- function(input, k=1){

  scatterplot <- dat <- xData <- yData <- xSelection <- ySelection <-
    logx <- logy <- flipX <- flipY <- col.pal <- outlier.var <- cutoff <- tail <-
    outlier.col.bg <- outlier.col <- outlier.transp <- outlier.pch <- outlier.cex <- n.bins <- NULL

  k <- as.character(k)

  ## Get x-axis & y-axis
  xSelection <- eval(parse(text=paste("input$scatterPlot_xaxis", k, sep="_")))
  ySelection <- eval(parse(text=paste("input$scatterPlot_yaxis", k, sep="_")))

  logx <- eval(parse(text=paste("input$scatterPlot_logx", k, sep="_")))
  logy <- eval(parse(text=paste("input$scatterPlot_logy", k, sep="_")))
  flipX <- eval(parse(text=paste("input$scatterPlot_flipx", k, sep="_")))
  flipY <- eval(parse(text=paste("input$scatterPlot_flipy", k, sep="_")))


   ## Get data and plot output
  if(!is.null(data_outliers())){
    if(!is.null(xSelection) && !is.null(ySelection)){

      ## Get data
      dat <- data_outliers()

  if(logx=="none"){logx=NULL}else{
    if(sum(xSelection<0)>0){print("Error: You are trying to log-transform
                                  negative values in the X variable.
                                  These values will not be plotted.")}
    if(logx=="log2"){logx=2}
    if(logx=="log10"){logx=10}
  }

  ## Log y-axis?
  if(logy=="none"){logy=NULL}else{
    if(sum(ySelection<0)>0){print("Error: You are trying to log-transform
                                  negative values in the Y variable.
                                  These values will not be plotted.")}
    if(logy=="log2"){logy=2}
    if(logy=="log10"){logy=10}
  }

  ## Invert x-axis?
  if(flipX=="No"){flipX=1}else{
    if(flipX=="Yes"){flipX=-1}}

  ## Invert y-axis?
  if(flipY=="No"){flipY=1}else{
    if(flipY=="Yes"){flipY=-1}}

  #########################
  ## Get plot aesthetics ##
  #########################

  ## Get plot aesthetics
  col.pal <- eval(parse(text=paste("input$scatterPlot_col.pal", k, sep="_")))
  n.bins <- eval(parse(text=paste("input$scatterPlot_n.bins", k, sep="_")))
  grid <- eval(parse(text=paste("input$scatterPlot_grid", k, sep="_")))

  outlier.col.bg <- eval(parse(text=paste("input$scatterPlot_outlier.col.bg", k, sep="_")))
  outlier.col <- eval(parse(text=paste("input$scatterPlot_outlier.col", k, sep="_")))
  outlier.transp <- eval(parse(text=paste("input$scatterPlot_outlier.transp", k, sep="_")))
  outlier.pch <- as.numeric(eval(parse(text=paste("input$scatterPlot_outlier.pch", k, sep="_"))))
  outlier.cex <- eval(parse(text=paste("input$scatterPlot_outlier.cex", k, sep="_")))

  ## Get outlier var
  outlier.var <- eval(parse(text=paste("input$scatterPlot_outlier.var", k, sep="_")))
  cutoff <- as.numeric(eval(parse(text=paste("input$scatterPlot_outlier.cutoff", k, sep="_"))))
  tail <- eval(parse(text=paste("input$scatterPlot_outlier.tail", k, sep="_")))

  n <- 100
  start <- 0.25
  end <- 0.9
  alpha <- 1
  if(col.pal == "gray.colors"){
    col.pal <- eval(parse(text=paste(col.pal, "(n=n, start=start, end=end)", sep="")))
  }else{
    col.pal <- eval(parse(text=paste(col.pal, "(n=n, alpha=alpha)", sep="")))
  }




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

      ## get log of x and y variables:
      toRemove <- toRemoveX <- toRemoveY <- NULL
      if(length(logx) == 1){
        toRemoveX <- which(xData <= 0)
      }
      if(length(logy) == 1){
        toRemoveY <- which(yData <= 0)
      }
      toRemove <- c(toRemoveX, toRemoveY)

      xData <- replace(xData, toRemove, NA)
      yData <- replace(yData, toRemove, NA)

      if(length(logx)==1){xData=log(xData+1e-40, logx)}
      if(length(logy)==1){yData=log(yData+1e-40, logy)}

      if(is.na(cutoff)){cutoff=0.01}
      if(tail=="Upper"){
        cutoff=(1-cutoff)
      }
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
      if(tail=="Two-tailed"){
        xData_sub_l <- xData[outlier.DataNew2<=cutoff]
        yData_sub_l <- yData[outlier.DataNew2<=cutoff]

        cutoff <- (1-cutoff)
        xData_sub_u <- xData[outlier.DataNew2>=cutoff]
        yData_sub_u <- yData[outlier.DataNew2>=cutoff]

        xData_sub <- c(xData_sub_l, xData_sub_u)
        yData_sub <- c(yData_sub_l, yData_sub_u)
      }


      xData <- xData*flipX
      yData <- yData*flipY
      xData_sub <- xData_sub*flipX
      yData_sub <- yData_sub*flipY

      # get colors
      #          get.levels <- levels(as.factor(colData))
      #          n.levels <- length(get.levels)
      #          colIndex <- as.numeric(as.factor(colData))
      #           if(!(colPal=="black")){
      #            myCol <- get(colPal)(n.levels)[colIndex]
      #            }else(myCol <- rgb(0,0,0,0.2))

      # produce plot
      #scatterplot <- plot(xData, yData, xlab=xSelection, ylab=ySelection, col=myCol, pch=20)
      scatterplot <- plot_2D(xData, yData,
                             xlab=xSelection, ylab=ySelection,
                             n.bins=n.bins,
                             col.pal=col.pal, grid=grid,
                             outlier.x=xData_sub, outlier.y=yData_sub,
                             outlier.col=outlier.col, outlier.col.bg=outlier.col.bg,
                             outlier.transp=outlier.transp,
                             outlier.pch=outlier.pch, outlier.cex=outlier.cex)

    }
  }
  return(scatterplot)
  # scatterplot
} # end .get.scatterPlot






