
################################
## LINEAR MANHATTAN PLOT PAGE ##  ------------------------------------------------------------------------------------
################################

#' @importFrom shiny reactiveValues
#' @importFrom shiny actionButton
#' @importFrom shinydashboard box
#' @importFrom shiny renderUI
#' @importFrom shiny fluidRow
#' @importFrom shiny selectizeInput
#' @importFrom shiny sliderInput
#' @importFrom shiny radioButtons
#' @importFrom shiny hr
#' @importFrom shiny actionButton
#' @importFrom shiny renderPlot
#' @importFrom shiny plotOutput
#' @importFrom ash::bin2
#' @importFrom shiny h5
#' @importFrom shiny p
#' @importFrom shiny textInput
#' @importFrom adegenet transp
#' @importFrom fields image.plot

#'
#'
## generate reactiveValues lists for all initial values

## variables
rv_linearManhattan_button <- shiny::reactiveValues()
rv_linearManhattan_button <- 1 # 0
# rv_linearManhattan_xaxis <- shiny::reactiveValues()
rv_linearManhattan_yaxis <- shiny::reactiveValues()
# rv_linearManhattan_logx <- shiny::reactiveValues()
rv_linearManhattan_logy <- shiny::reactiveValues()
# rv_linearManhattan_flipx <- shiny::reactiveValues()
rv_linearManhattan_flipy <- shiny::reactiveValues()

rv_linearManhattan_outlier.var <- shiny::reactiveValues()
rv_linearManhattan_outlier.cutoff <- shiny::reactiveValues()
rv_linearManhattan_outlier.tail <- shiny::reactiveValues()

## aesthetics
rv_linearManhattan_col.pal <- shiny::reactiveValues()
rv_linearManhattan_n.bins <- shiny::reactiveValues()
rv_linearManhattan_grid <- shiny::reactiveValues()

rv_linearManhattan_outlier.col.bg <- shiny::reactiveValues()
rv_linearManhattan_outlier.col <- shiny::reactiveValues()
rv_linearManhattan_outlier.pch <- shiny::reactiveValues()
rv_linearManhattan_outlier.transp <- shiny::reactiveValues()
rv_linearManhattan_outlier.cex <- shiny::reactiveValues()






#########################################
## .set.reactiveValues.linearManhattan ##
#########################################
## fn to set reactiveValues initially for each k:
.set.reactiveValues.linearManhattan <- function(dat, k){

  k <- as.character(k)

  x.var.choices <- x.var.sel <-
    y.var.choices <- y.var.sel <-
    o.var.choices <- o.var.sel <- NULL

  ## get variables
  if(!is.null(dat)){

    ## get chromosome and position variables:
    # x.var.choices <- c("Position", "Chromosome") # c(names(dat$pos), names(dat$chrom))

    ## get numeric variables
    numCols <- which(sapply(c(1:ncol(dat$y)),
                            function(e) is.numeric(dat$y[,e])))

    var.choices <- c(names(dat$y)[numCols])

    y.var.choices <- o.var.choices <- var.choices

    # x.var.sel <- x.var.choices[1]

    y.var.sel <- y.var.choices[1]
    o.var.sel <- o.var.choices[2]

    ## set intial values
    # rv_linearManhattan_xaxis[[k]] <- x.var.sel
    rv_linearManhattan_yaxis[[k]] <- y.var.sel
    # rv_linearManhattan_logx[[k]] <- "none"
    rv_linearManhattan_logy[[k]] <- "none"
    # rv_linearManhattan_flipx[[k]] <- "No"
    rv_linearManhattan_flipy[[k]] <- "No"
    rv_linearManhattan_outlier.var[[k]] <- o.var.sel
    rv_linearManhattan_outlier.cutoff[[k]] <- 0.05
    rv_linearManhattan_outlier.tail[[k]] <- "Lower"

    rv_linearManhattan_col.pal[[k]] <- "heat.colors"
    rv_linearManhattan_n.bins[[k]] <- 100
    rv_linearManhattan_grid[[k]] <- FALSE

    rv_linearManhattan_outlier.col.bg[[k]] <- "purple"
    rv_linearManhattan_outlier.col[[k]] <- "blue"
    rv_linearManhattan_outlier.pch[[k]] <- "24"
    rv_linearManhattan_outlier.transp[[k]] <- 0.25
    rv_linearManhattan_outlier.cex[[k]] <- 1.5

  }
} # end .set.reactiveValues.linearManhattan



############################################
## .update.reactiveValues.linearManhattan ##
############################################
## fn to set reactiveValues initially for each k:
.update.reactiveValues.linearManhattan <- function(dat, k){

  k <- as.character(k)

  x.var.choices <- x.var.sel <-
    y.var.choices <- y.var.sel <-
    o.var.choices <- o.var.sel <- NULL

  ## get variables
  if(!is.null(dat)){

    ## Get currently-selected values:

    ## Get x-axis & y-axis
    # xSelection <- eval(parse(text=paste("input$linearManhattan_xaxis", k, sep="_")))
    ySelection <- eval(parse(text=paste("input$linearManhattan_yaxis", k, sep="_")))

    # logx <- eval(parse(text=paste("input$linearManhattan_logx", k, sep="_")))
    logy <- eval(parse(text=paste("input$linearManhattan_logy", k, sep="_")))
    # flipX <- eval(parse(text=paste("input$linearManhattan_flipx", k, sep="_")))
    flipY <- eval(parse(text=paste("input$linearManhattan_flipy", k, sep="_")))

    ## Get plot aesthetics
    col.pal <- eval(parse(text=paste("input$linearManhattan_col.pal", k, sep="_")))
    n.bins <- eval(parse(text=paste("input$linearManhattan_n.bins", k, sep="_")))
    grid <- eval(parse(text=paste("input$linearManhattan_grid", k, sep="_")))

    outlier.col.bg <- eval(parse(text=paste("input$linearManhattan_outlier.col.bg", k, sep="_")))
    outlier.col <- eval(parse(text=paste("input$linearManhattan_outlier.col", k, sep="_")))
    outlier.transp <- eval(parse(text=paste("input$linearManhattan_outlier.transp", k, sep="_")))
    outlier.pch <- eval(parse(text=paste("input$linearManhattan_outlier.pch", k, sep="_")))
    outlier.cex <- eval(parse(text=paste("input$linearManhattan_outlier.cex", k, sep="_")))



    ## Get outlier var
    outlier.var <- eval(parse(text=paste("input$linearManhattan_outlier.var", k, sep="_")))
    cutoff <- eval(parse(text=paste("input$linearManhattan_outlier.cutoff", k, sep="_")))
    tail <- eval(parse(text=paste("input$linearManhattan_outlier.tail", k, sep="_")))

    ## update "intial" values to current values
    # rv_linearManhattan_xaxis[[k]] <- xSelection
    rv_linearManhattan_yaxis[[k]] <- ySelection
    # rv_linearManhattan_logx[[k]] <- logx
    rv_linearManhattan_logy[[k]] <- logy
    # rv_linearManhattan_flipx[[k]] <- flipX
    rv_linearManhattan_flipy[[k]] <- flipY
    rv_linearManhattan_outlier.var[[k]] <- outlier.var
    rv_linearManhattan_outlier.cutoff[[k]] <- cutoff
    rv_linearManhattan_outlier.tail[[k]] <- tail

    rv_linearManhattan_n.bins[[k]] <- n.bins
    rv_linearManhattan_col.pal[[k]] <- col.pal
    rv_linearManhattan_grid[[k]] <- grid

    rv_linearManhattan_outlier.col.bg[[k]] <- outlier.col.bg
    rv_linearManhattan_outlier.col[[k]] <- outlier.col
    rv_linearManhattan_outlier.pch[[k]] <- outlier.pch
    rv_linearManhattan_outlier.transp[[k]] <- outlier.transp
    rv_linearManhattan_outlier.cex[[k]] <- outlier.cex

  }
} # end .update.reactiveValues.linearManhattan



## update K & set reactiveValues[[k]] if button pressed
observe({

  k <- input$new_linearManhattan_button

  if(length(k) == 1){
    k <- k[1]+1
    ## if input button updates, set new panel of initial input values

    dat <- data_outliers()

    ## if K updates:
    if(!is.null(dat)){

      if(k == 1){
        .set.reactiveValues.linearManhattan(dat, k)
      }else{
        if(k > rv_linearManhattan_button){
          ## update rv_linearManhattan_button
          rv_linearManhattan_button <- k

          # set reactive values for Kth element of rv lists
          .set.reactiveValues.linearManhattan(dat, k)
          # .update.reactiveValues.linearManhattan(dat, k)

          ## if more than one panel requested, update "initial" values for plots 1:k-1
          if(k > 1){
            for(i in 1:(k-1)){
              .update.reactiveValues.linearManhattan(dat, i)
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
output$box_linearManhattan <- shiny::renderUI({

  k <- 1
  k <- input$new_linearManhattan_button[1] + 1

  if(length(k) > 0){
    if(k > 0){
      lapply(1:k,function(i){

        dat <- title.k <- NULL

        ## get title
        title.k <- paste("Linear Manhattan Plot #", i, sep = " ")

        ## get data
        dat <- data_outliers()

        ## get box of boxes
        if(!is.null(dat)){

          shinydashboard::box(
            title=title.k,
            status="warning",
            # status="danger",
            solidHeader=TRUE,
            collapsible=TRUE,
            width=12,
            # background="teal",

            shiny::fluidRow(
              column(4,
                     .get.linearManhattan.controls(dat, i)
              ),

              # fluidRow(
              column(8,
                     .get.linearManhattan.plot(dat, i),
                     .get.linearManhattan.controls.aes(dat, i)
              )
            ),
            style = list('background-color: #B6B6B6') # dark gray (amber)
            # style = list('background-color: #727272') # dark gray (light blue)

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

.get.linearManhattan.controls <- function(dat, k=1){

  k <- as.character(k)

  out <- NULL

  ## get variables
  if(!is.null(dat)){

    # x.var.choices <-  c("Position", "Chromosome") # c(names(dat$y$pos), names(dat$y$chrom))

    ## get numeric variables
    numCols <- which(sapply(c(1:ncol(dat$y)),
                            function(e) is.numeric(dat$y[,e])))

    var.choices <- c(names(dat$y)[numCols])

    y.var.choices <- o.var.choices <- var.choices

  }

  ## get id's | k
  # k <- 1
  # id_linearManhattan_xaxis <- paste("linearManhattan_xaxis", k, sep="_")
  # id_linearManhattan_logx <- paste("linearManhattan_logx", k, sep="_")
  # id_linearManhattan_flipx <- paste("linearManhattan_flipx", k, sep="_")
  id_linearManhattan_yaxis <- paste("linearManhattan_yaxis", k, sep="_")
  id_linearManhattan_logy <- paste("linearManhattan_logy", k, sep="_")
  id_linearManhattan_flipy <- paste("linearManhattan_flipy", k, sep="_")
  id_linearManhattan_outlier.var <- paste("linearManhattan_outlier.var", k, sep="_")
  id_linearManhattan_outlier.cutoff <- paste("linearManhattan_outlier.cutoff", k, sep="_")
  id_linearManhattan_outlier.tail <- paste("linearManhattan_outlier.tail", k, sep="_")

  out <-
    shinydashboard::box(
      title="Select Variables:", # "Univariate Distributions"
      status="warning",
      # status="primary",
      solidHeader=FALSE,
      collapsible=TRUE,
      width=12,
      # background="teal",

      ###################
      ## Choose x-axis ##
      ###################

      #         shinydashboard::box(title="Adjust x-axis:", # "Univariate Distributions"
      #             # status="info",
      #             status = "warning",
      #             # status="primary",
      #             solidHeader=TRUE,
      #             collapsible=TRUE,
      #             width=12,
      #
      #
      #             ## NOTE: Would like to be able to pull the Chromosome and Position variables
      #             ## selected/generated in the Format Data tab to be available as options
      #             ## and autoatically selected below...
      #
      #             ## Choose x-axis variable
      #             h5(strong("X-axis:")),
      #             helpText("Note: The x-axis used in this Manhattan plot is the 'Position' variable
      #               selected in the 'Format Data' tab on the 'Data' page."),

      #             selectizeInput(id_linearManhattan_xaxis,
      #                            label = 'X-axis:',
      #                            choices = x.var.choices,
      #                            selected = rv_linearManhattan_xaxis[[k]], # x.var.sel,
      #                            multiple = FALSE),

      ## log(x-axis) ?
      #             radioButtons(id_linearManhattan_logx,
      #                          label = "Log x-axis?",
      #                          choices = list("log2", "log10", "none"),
      #                          selected= rv_linearManhattan_logx[[k]], # "none",
      #                          inline=TRUE),

      #             ## Flip x-axis ?
      #             radioButtons(id_linearManhattan_flipx,
      #                          label = "Invert x-axis?",
      #                          choices = list("Yes", "No"),
      #                          selected= rv_linearManhattan_flipx[[k]], # "No",
      #                          inline=TRUE),
      #
      #             style = list('background-color: #FFECB3') # pale amber
      #             # style = list('background-color: #B2EBF2') # pale cyan
      #             # style = list('background-color: #B3E5FC') # pale light blue
      #         ),

      ###################
      ## Choose y-axis ##
      ###################

      shinydashboard::box(
        title="Select y-axis:",
        # status="info",
        status = "warning",
        # status="primary",
        solidHeader=TRUE,
        collapsible=TRUE,
        width=12,

        ## Choose y-axis variable
        shiny::selectizeInput(
          id_linearManhattan_yaxis,
          label = 'Y-axis:',
          choices = y.var.choices,
          selected =  rv_linearManhattan_yaxis[[k]], # y.var.sel,
          multiple = FALSE
        ),

        ## log(y-axis) ?
        shiny::radioButtons(
          id_linearManhattan_logy,
          label = "Log y-axis?",
          choices = list("log2", "log10", "none"),
          selected= rv_linearManhattan_logy[[k]], # "none",
          inline=TRUE
        ),

        ## Flip y-axis ?
        shiny::radioButtons(
          id_linearManhattan_flipy,
          label = "Invert y-axis?",
          choices = list("Yes", "No"),
          selected= rv_linearManhattan_flipy[[k]], # "No",
          inline=TRUE
        ),

        style = list('background-color: #FFECB3') # pale amber
        # style = list('background-color: #B2EBF2') # pale cyan
        # style = list('background-color: #B3E5FC') # pale light blue

      ),


      ###############################################
      ## Choose outlier variable (usually p-value) ##
      ###############################################

      ## NOTE: I'm not 100% sure what the best way to refer to this variable is...
      ## ie. "Second variable" or "Outlier detection variable" or "Univariate outlier detection variable"??

      shinydashboard::box(
        title="Select outlier variable:",
        # status="info",
        status = "warning",
        # status="primary",
        solidHeader=TRUE,
        collapsible=TRUE,
        width=12,

        ## Mark outliers by second variable (usually p-value)
        shiny::h5(strong('Highlight outliers by this variable:')),
        shiny::p("For example, you may wish to identify outliers according to a p-value
              that is recorded in another column of the data table."),
        shiny::selectizeInput(
          id_linearManhattan_outlier.var,
          label = NULL,
          choices = o.var.choices,
          selected = rv_linearManhattan_outlier.var[[k]], # o.var.sel,
          multiple = FALSE
        ),

        ## Cut-off for outliers to overlay
        # eg 0.01
        shiny::textInput(
          id_linearManhattan_outlier.cutoff,
          label = "Cut-off for outliers to overlay",
          value =  rv_linearManhattan_outlier.cutoff[[k]] # 0.05
        ),

        shiny::radioButtons(
          id_linearManhattan_outlier.tail,
          label = "Tail",
          choices = c("Lower", "Upper", "Two-tailed"),
          selected =  rv_linearManhattan_outlier.tail[[k]], # "Lower",
          inline=TRUE
        ),

        style = list('background-color: #FFECB3') # pale amber
        # style = list('background-color: #B2EBF2') # pale cyan
        # style = list('background-color: #B3E5FC') # pale light blue
      )

      # style = list('background-color: #FFECB3')
      # style = list('background-color: #FFFFFF') # pale gray (light blue)

    )

  return(out)
} # end .get.linearManhattan.controls





#######################################
## .get.linearManhattan.controls.aes ##
#######################################
## fn to get widgets to control plot AESTHETICS under plot
.get.linearManhattan.controls.aes <- function(dat, k=1){

  k <- as.character(k)

  ## get Id's | k
  id_linearManhattan_col.pal <- paste("linearManhattan_col.pal", k, sep="_")
  id_linearManhattan_n.bins <- paste("linearManhattan_n.bins", k, sep="_")
  id_linearManhattan_grid <- paste("linearManhattan_grid", k, sep="_")
  id_linearManhattan_outlier.col.bg <- paste("linearManhattan_outlier.col.bg", k, sep="_")
  id_linearManhattan_outlier.col <- paste("linearManhattan_outlier.col", k, sep="_")
  id_linearManhattan_outlier.pch <- paste("linearManhattan_outlier.pch", k, sep="_")
  id_linearManhattan_outlier.transp <- paste("linearManhattan_outlier.transp", k, sep="_")
  id_linearManhattan_outlier.cex <- paste("linearManhattan_outlier.cex", k, sep="_")


  out <- NULL

  out <-
    shinydashboard::box(
      title="Adjust Plot Aesthetics:",
      status="warning",
      # status="primary",
      solidHeader=FALSE,
      collapsible=TRUE,
      width=12,
      # background="yellow",

      # h4("Scatter aesthetics:"),
      shinydashboard::box(
        title="Scatter aesthetics:",
        status="warning",
        # status="primary",
        solidHeader=TRUE,
        collapsible=TRUE,
        width=12,


        shiny::fluidRow(
          column(4,
                 ## selectInput w col.pals
                 shiny::selectizeInput(
                   id_linearManhattan_col.pal,
                   label="Colour palette:",
                   choices = list("Heat colours" = "heat.colors",
                                  "Terrain colours" = "terrain.colors",
                                  "Topo colours" = "topo.colors",
                                  "CM colours" = "cm.colors",
                                  "Gray colours" = "gray.colors"),
                   selected =  rv_linearManhattan_col.pal[[k]], # "heat.colors",
                   multiple=FALSE)),

          column(4,
                 shiny::sliderInput(
                   id_linearManhattan_n.bins,
                   label = "Number of bins:",
                   min = 2, max = 1000,
                   value =  rv_linearManhattan_n.bins[[k]], # 100,
                   step = 1
                 )
          ),

          column(4,
                 shiny::radioButtons(
                   id_linearManhattan_grid,
                   label="Overlay grid?",
                   choices=list("Yes" = TRUE,
                                "No" = FALSE),
                   selected = rv_linearManhattan_grid[[k]],
                   inline = TRUE
                 )
          )
        ),

        style = list('background-color: #FFECB3') # pale amber
        # style = list('background-color: #B2EBF2') # pale cyan
        # style = list('background-color: #B3E5FC') # pale light blue
      ),



      shinydashboard::box(
        title="Outlier aesthetics:",
        status="warning",
        # status="primary",
        solidHeader=TRUE,
        collapsible=TRUE,
        width=12,

        shiny::fluidRow(
          column(4,
                 shiny::selectizeInput(
                   id_linearManhattan_outlier.col.bg,
                   label = "Outlier colour (fill):",
                   choices = list("Red" = "red",
                                  "Orange" = "orange",
                                  "Yellow" = "yellow",
                                  "Green" = "green",
                                  "Blue" = "blue",
                                  "Purple" = "purple"),
                   selected =  rv_linearManhattan_outlier.col.bg[[k]], # "purple",
                   multiple=FALSE)),

          column(4,
                 shiny::selectizeInput(
                   id_linearManhattan_outlier.col,
                   label = "Outlier colour (outline):",
                   choices = list("Red" = "red",
                                  "Orange" = "orange",
                                  "Yellow" = "yellow",
                                  "Green" = "green",
                                  "Blue" = "blue",
                                  "Purple" = "purple"),
                   selected =  rv_linearManhattan_outlier.col[[k]], # "blue",
                   multiple=FALSE)),

          column(4,
                 shiny::selectizeInput(
                   id_linearManhattan_outlier.pch,
                   label = "Outlier shape:",
                   choices = list("Circle" = "21",
                                  "Square" = "22",
                                  "Diamond" = "23",
                                  "Triangle, point-up" = "24",
                                  "Triangle, point-down" = "25"
                   ),
                   selected =  rv_linearManhattan_outlier.pch[[k]], # "24",
                   multiple=FALSE))
        ),

        shiny::hr(),

        shiny::fluidRow(
          column(6,
                 shiny::sliderInput(
                   id_linearManhattan_outlier.transp,
                   label = "Outlier transparency:",
                   min = 0, max = 1,
                   value =  rv_linearManhattan_outlier.transp[[k]], # 0.25,
                   step = 0.05)),

          column(6,
                 shiny::sliderInput(
                   id_linearManhattan_outlier.cex,
                   label = "Outlier size:",
                   min = 0, max = 3,
                   value =  rv_linearManhattan_outlier.cex[[k]], # 1.5,
                   step = 0.1))
        ),

        style = list('background-color: #FFECB3') # pale amber
        # style = list('background-color: #B2EBF2') # pale cyan
        # style = list('background-color: #B3E5FC') # pale light blue
      )

      # style = list('background-color: #FFFFFF') # pale gray (light blue)

    ) # end box

  return(out)

} # end .get.linearManhattan.controls.aes



####################################
## BUTTON: Generate another plot? ##
####################################
output$box_linearManhattan_button <- shiny::renderUI({
  shinydashboard::box(
    title = "Generate another plot?",
    solidHeader = TRUE,
    status = "primary",
    value = NULL,
    width=12,

    ## button
    shiny::actionButton(
      inputId = "new_linearManhattan_button",
      label = "Yes, please!",
      icon = icon("cog")
    )
  )
})




##############################
## get.linearManhattan.plot ##
##############################
.get.linearManhattan.plot <- function(dat, k=1){

  out <- NULL

  if(!is.null(k)){

    ## get unique outputId
    id_linearManhattan <- paste("id_linearManhattan", k, sep="_")

    out <-
      shinydashboard::box(
        title=NULL,
        status="warning",
        solidHeader=FALSE,
        collapsible=TRUE,
        width=12,
        # plotOutput("plot_linearManhattan_plot")
        shiny::renderPlot(
          shiny::plotOutput(
            outputId = id_linearManhattan,
            .get.linearManhattan(input, k=k)
          )
        )
      )
  }
  return(out)
}
# end .get.linearManhattan.plot

######################################################################################################################

###########################
## Linear Manhattan Plot ##
###########################

# #######################
# ## .getLinearMHTPlot ##
# #######################
# .getLinearMHTPlot <- function(mainData, input){
#   xchr = input$choose_xaxis_chr
#   xcood = input$choose_xaxis_cood
#   yselect = input$choose_y1_plot
#   logy = input$logy1Checkbox
#   pselect = input$choose_pval
#   poutlier = as.numeric(input$linearmhtpcut)
#   flipYaxis = input$flipY
#   n_bins <- as.numeric(as.character(input$linearmht_nbins))
#
#   mhtplots <- NULL
#   if(!is.null(mainData)){
#     if(!is.null(yselect) && !is.null(pselect)){
#       mhtplots <- .mhtplot(mydata=mainData, Chr=xchr, BP = xcood, ycolnam=yselect, pcolnam=pselect,
#                            pcut.outlier= poutlier, logY=logy, nbins=n_bins, flipY=flipYaxis)
#     }
#   }
#   mhtplots
# } # end .getLinearMHTPlot



######################################################################################################################





##########################
## .get.linearManhattan ##
##########################
.get.linearManhattan <- function(input, k=1){

  linearManhattan <- dat <- xData <- yData <- xSelection <- ySelection <-
    logx <- logy <- flipX <- flipY <- col.pal <- outlier.var <- cutoff <- tail <-
    outlier.col.bg <- outlier.col <- outlier.transp <- outlier.pch <- outlier.cex <- n.bins <- NULL

  k <- as.character(k)

  ## Get x-axis & y-axis
  ySelection <- eval(parse(text=paste("input$linearManhattan_yaxis", k, sep="_")))

  # logx <- eval(parse(text=paste("input$linearManhattan_logx", k, sep="_")))
  logy <- eval(parse(text=paste("input$linearManhattan_logy", k, sep="_")))
  # flipX <- eval(parse(text=paste("input$linearManhattan_flipx", k, sep="_")))
  flipY <- eval(parse(text=paste("input$linearManhattan_flipy", k, sep="_")))


  ## Get data and plot output
  if(!is.null(data_outliers())){
    if(!is.null(ySelection)){

      ## Get data
      dat <- data_outliers()

      xSelection <- dat$pos
      #
      #       if(logx=="none"){logx=NULL}else{
      #         if(sum(xSelection<0)>0){print("Error: You are trying to log-transform
      #                                       negative values in the X variable.
      #                                       These values will not be plotted.")}
      #         if(logx=="log2"){logx=2}
      #         if(logx=="log10"){logx=10}
      #         }

      logx <- NULL

      ## Log y-axis?
      if(logy=="none"){logy=NULL}else{
        if(sum(ySelection<0)>0){
          print("Error: You are trying to log-transform
                                      negative values in the Y variable.
                                      These values will not be plotted.")
        }
        if(logy=="log2"){logy=2}
        if(logy=="log10"){logy=10}
      }

      ## Invert x-axis?
      #       if(flipX=="No"){flipX=1}else{
      #         if(flipX=="Yes"){flipX=-1}}

      flipX <- 1

      ## Invert y-axis?
      if(flipY=="No"){flipY=1}else{
        if(flipY=="Yes"){flipY=-1}}

      #########################
      ## Get plot aesthetics ##
      #########################

      ## Get plot aesthetics
      col.pal <- eval(parse(text=paste("input$linearManhattan_col.pal", k, sep="_")))
      n.bins <- eval(parse(text=paste("input$linearManhattan_n.bins", k, sep="_")))
      grid <- eval(parse(text=paste("input$linearManhattan_grid", k, sep="_")))

      outlier.col.bg <- eval(parse(text=paste("input$linearManhattan_outlier.col.bg", k, sep="_")))
      outlier.col <- eval(parse(text=paste("input$linearManhattan_outlier.col", k, sep="_")))
      outlier.transp <- eval(parse(text=paste("input$linearManhattan_outlier.transp", k, sep="_")))
      outlier.pch <- as.numeric(eval(parse(text=paste("input$linearManhattan_outlier.pch", k, sep="_"))))
      outlier.cex <- eval(parse(text=paste("input$linearManhattan_outlier.cex", k, sep="_")))

      ## Get outlier var
      outlier.var <- eval(parse(text=paste("input$linearManhattan_outlier.var", k, sep="_")))
      cutoff <- as.numeric(eval(parse(text=paste("input$linearManhattan_outlier.cutoff", k, sep="_"))))
      tail <- eval(parse(text=paste("input$linearManhattan_outlier.tail", k, sep="_")))

      n <- 100
      start <- 0.25
      end <- 0.9
      alpha <- 1
      if(col.pal == "gray.colors"){
        col.pal <- eval(parse(text=paste(col.pal, "(n=n, start=start, end=end)", sep="")))
      }else{
        col.pal <- eval(parse(text=paste(col.pal, "(n=n, alpha=alpha)", sep="")))
      }


      ## Get X, Chr, Pos variables
      ## Get Pos
      Pos <- eval(parse(text="dat$pos"))

      ## Get Chr
      Chr <- eval(parse(text="dat$chrom"))

      ## Get x-variable data
      ## ie. EITHER POS OR CHROMOSOME...
      if(!is.null(xSelection)){
        ## Get variable to plot
        xData <- xSelection # dat[,Pos] # eval(parse(text=paste("dat", xSelection, sep="$")))
      }
      ## Get x-variable data
      ## ie. VARIABLE TO PLOT
      if(!is.null(ySelection)){
        ## Get variable to plot
        yData <- eval(parse(text=paste("dat$y", ySelection, sep="$")))
      }


      ## Get outlier-variable data
      if(!is.null(outlier.var)){
        ## Get variable to plot
        outlier.Data <- eval(parse(text=paste("dat$y", outlier.var, sep="$")))
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

      if(length(logx)==1){
        xData <- log(xData+1e-40, logx)
      }
      if(length(logy)==1){
        yData <- log(yData+1e-40, logy)
      }

      if(is.na(cutoff)){cutoff=0}
      if(tail=="Upper"){
        cutoff=(1-cutoff)
      }
      outlier.DataNoNA <- outlier.Data[!is.na(outlier.Data)]
      outlier.DataNew <- rank(outlier.DataNoNA)/length(outlier.DataNoNA)
      outlier.DataNew2 <- outlier.Data
      outlier.DataNew2[!is.na(outlier.Data)] <- outlier.DataNew

      ## handle outliers | log of x, y
      outlier.Data <- outlier.DataNew2

      if(tail=="Lower"){
        xData_sub <- xData[outlier.Data<=cutoff]
        yData_sub <- yData[outlier.Data<=cutoff]
      }
      if(tail=="Upper"){
        xData_sub <- xData[outlier.Data>=cutoff]
        yData_sub <- yData[outlier.Data>=cutoff]
      }
      if(tail=="Two-tailed"){
        xData_sub_l <- xData[outlier.Data<=cutoff]
        yData_sub_l <- yData[outlier.Data<=cutoff]

        cutoff <- (1-cutoff)
        xData_sub_u <- xData[outlier.Data>=cutoff]
        yData_sub_u <- yData[outlier.Data>=cutoff]

        xData_sub <- c(xData_sub_l, xData_sub_u)
        yData_sub <- c(yData_sub_l, yData_sub_u)
      }

      xData_sub <- xData_sub[!is.na(xData_sub)]
      yData_sub <- yData_sub[!is.na(yData_sub)]

      xData <- xData*flipX
      yData <- yData*flipY
      xData_sub <- xData_sub*flipX
      yData_sub <- yData_sub*flipY

      # produce plot
      linearManhattan <- .mhtplot(
        x=xData, y=yData,
        xlab="Position", ylab=ySelection,
        n.bins=n.bins, x_sub=xData_sub, y_sub=yData_sub,
        col.pal=col.pal, grid=grid,
        outlier.col=outlier.col, outlier.col.bg=outlier.col.bg,
        outlier.transp=outlier.transp,
        outlier.pch=outlier.pch, outlier.cex=outlier.cex
      )
    }
  }

  return(linearManhattan)
  # linearManhattan
} # end .get.linearManhattan




##############
## .mhtplot ##
##############

### Plot function, need to move to function area

.mhtplot <- function(
  x, y,
  xlab, ylab,
  xlim=NULL, ylim=NULL,
  n.bins,
  x_sub, y_sub,
  col.pal, grid,
  outlier.col, outlier.col.bg,
  outlier.transp,
  outlier.pch, outlier.cex
){

  if(outlier.transp != 0){
    outlier.transp <- 1 - outlier.transp
    outlier.col <- adegenet::transp(outlier.col, alpha = outlier.transp)
    outlier.col.bg <- adegenet::transp(outlier.col.bg, alpha = outlier.transp)
  }

  data1 <- cbind(x, y)
  data1b <- data1[complete.cases(data1),]

  # plot(1)

  if(length(xlim)==0){
    xlim_up <- max(x, na.rm=TRUE)
    xlim_lower <- min(x, na.rm=TRUE)
  }

  if(length(ylim)==0){
    ylim_up <- max(y, na.rm=TRUE)
    ylim_lower <- min(y, na.rm=TRUE)
  }

  binned <- ash::bin2(
    data1b,
    matrix(c(xlim_lower,xlim_up,ylim_lower,ylim_up), 2,2, byrow=TRUE),
    nbin=c(n.bins,n.bins)
  )
  binned$nc[binned$nc==0]=NA

  x.axis.min <- xlim_lower
  x.axis.max <- xlim_up
  y.axis.min <- ylim_lower
  y.axis.max <- ylim_up


  dat <- data_outliers()
  mynewtoy <- split(dat, "chrom") # split(mydata, mydata[,Chr])
  chrs.max <- lapply(sapply(mynewtoy,'[',"pos"),max) # lapply(sapply(mynewtoy,'[',BP),max)
  x.total <- cumsum(as.numeric(unlist(chrs.max)))
  x.axis.scale<-300/max(x.total)
  x.total2<-c(0,x.total)

  # plot(1)

  ## LINEAR MANHATTAN PLOT
  fields::image.plot(
    seq(x.axis.min,x.axis.max,length.out = n.bins),
    seq(y.axis.min, y.axis.max, length.out=n.bins),
    binned$nc,
    xlab=xlab, ylab=ylab, add=FALSE,
    col=col.pal, axes=TRUE
  )

  ## ADD OUTLIER POINTS
  points(x_sub, y_sub, pch=outlier.pch, cex=outlier.cex, col=outlier.col, bg=outlier.col.bg)
  # points(x=xaxis_all[data.outlier], y=yaxis_all[data.outlier], pch=18, cex=1,col="red")

  #   #axis(1, at=axTicks(1), label=T)
  #   axis(1,at=x.axis.scale*x.total2,labels=T)
  #   axis(1,at=x.axis.scale * x.total2[-1]-diff(x.axis.scale*x.total2)/2,
  #        labels=c(1:length(x.total)),cex=0.1,tick=F,cex.axis=0.8)
  #   axis(2,at=axTicks(2),label=T)

  ## ADD GRID
  if(grid) grid()

  ## SET TITLE TO VALUE BEING PLOTTED
  ## NOTE: to be changed to textInput( w x- and ySelection selected)!!!!!!
  if(!is.null(xlab) & !is.null(ylab)) title(paste(ylab, "by", xlab, sep=" "))

} # end .mhtplot



# ##########################
# ## Box: Navigation Plot ##
# ##########################
#
# # box for navigation plot
# output$box_linearManhattan_navigation <- shiny::renderUI({
#   shinydashboard::box(title="Manhattan Plot",
#       status="warning",
#       solidHeader=TRUE,
#       collapsible=TRUE,
#       width=12,
#       fluidRow(
#         column(6,
#                selectInput('restrict_chrom',
#                            label='Restrict chromosome',
#                            choices=as.list(c('(all)',
#                                              data_outliers()$chromLevels)),
#                            selected=navigationPolygons()$chromChosen, width=200)
#         )
#       ),
#       plotOutput('plot_navigation',height=100,width='100%'),
#       sliderInput('slider_navigate',
#                   label=NULL,
#                   min=navigationPolygons()$x_min,
#                   max=navigationPolygons()$x_max,
#                   value=c(navigationPolygons()$x_min, navigationPolygons()$x_max),
#                   step=1,
#                   width='100%'),
#       div(style="display:inline-block",
#           tags$button(id='apply_navigate_button', type="button",
#                       class="btn action-button btn-primary",
#                       style='font-size:15px; text-align:center',
#                       HTML('<i class="icon-star"></i>Apply Changes'))
#       )
#   ) # end of tabBox
# })
#
# # function evaluates when apply_navigate_button is pressed
# apply_navigate <- eventReactive(input$apply_navigate_button,{})
#
# # subset data_outliers() ready for Manhattan plot based on selectize_linearManhattan_variables
# ManhattanData <- reactive({
#   data <- data_outliers()
#   if (is.null(input$selectize_linearManhattan_variables)) {
#     output <- list(pos=data$pos,
#                    pos_modifier=data$pos_modifier,
#                    chrom=data$chrom,
#                    chromLevels=data$chromLevels,
#                    chromIndex=data$chromIndex,
#                    chromMidpoint=data$chromMidpoint,
#                    y=NULL,
#                    y_col=0,
#                    pos_userDefined=data$pos_userDefined,
#                    chrom_userDefined=data$chrom_userDefined)
#   } else {
#     y <- data$y[,input$selectize_linearManhattan_variables,drop=FALSE]
#     output <- list(pos=data$pos,
#                    pos_modifier=data$pos_modifier,
#                    chrom=data$chrom,
#                    chromLevels=data$chromLevels,
#                    chromIndex=data$chromIndex,
#                    chromMidpoint=data$chromMidpoint,
#                    y=y,
#                    y_col=ncol(y),
#                    pos_userDefined=data$pos_userDefined,
#                    chrom_userDefined=data$chrom_userDefined)
#   }
#   return(output)
# })
#
# ## reactive conductor for subsetting ManhattanData
# ## based on restrict_chrom selector, and converting into
# ## series of polygons for navigation plot.
# ## Returns:
# # # list(x_min,
# # #      x_max,
# # #      y_min,
# # #      y_max,
# # #      polygon=list(x,y,chromIndex),
# # #      chromLevels,
# # #      chromMidpoint)
#
#
# navigationPolygons <- reactive({
#
#   nullOutput <- list(x_min=0,
#                      x_max=100,
#                      y_min=0,
#                      y_max=1,
#                      polygon=list(),
#                      chromLevels=NULL,
#                      chromMidpoint=NULL,
#                      chromChosen='(all)')
#
#   data <- ManhattanData()
#
#   # if data$y is NULL, return nullOutput
#   if (is.null(data$y))
#     return(nullOutput)
#
#   # if input$restrict_chrom is NULL, return nullOutput
#   chromChosen <- input$restrict_chrom
#   if (is.null(chromChosen))
#     return(nullOutput)
#
#   # if plotting all chromosomes
#   if (chromChosen=='(all)') {
#     x <- data$pos + data$pos_modifier
#     y <- data$y[,1]
#     chromIndex <- data$chromIndex
#     chromLevels <- data$chromLevels
#
#   # if plotting single chromosome
#   } else {
#     x <- subset(data$pos,
#                 data$chrom==input$restrict_chrom)
#     y <- subset(data$y[,1],
#                 data$chrom==input$restrict_chrom)
#     chromIndex <- subset(data$chromIndex,
#                          data$chrom==input$restrict_chrom)
#     chromLevels <- input$restrict_chrom
#   }
#
#   ## convert x and y to polygons
#   polyNum <- 200
#   x_min <- min(x,na.rm=TRUE)
#   x_max <- max(x,na.rm=TRUE)
#   y_min <- min(y,na.rm=TRUE)
#   y_max <- max(y,na.rm=TRUE)
#
#   df <- data.frame(y=y, chromIndex=chromIndex)
#   breakVec <- seq(x_min, x_max, l=polyNum+1)
#   breakDelta <- (breakVec[2]-breakVec[1])/2
#   breakMids <- (breakVec[-1]+breakVec[-length(breakVec)])/2
#   ## split df into list based on which interval x falls into
#   c <- split(df,f=cut(x,breaks=breakVec))
#   output <- list(x_min=x_min,
#                  x_max=x_max,
#                  y_min=y_min,
#                  y_max=y_max,
#                  polygon=list(),
#                  chromLevels=chromLevels,
#                  chromMidpoint=data$chromMidpoint,
#                  chromChosen=chromChosen)
#   for (i in 1:length(c)) {
#     if (length(c[[i]]$y)>0) {
#       r <- range(c[[i]]$y,na.rm=TRUE)
#       output$polygon[[i]] <- list(x=c(breakMids[i]-breakDelta,
#                                       breakMids[i]+breakDelta,
#                                       breakMids[i]+breakDelta,
#                                       breakMids[i]-breakDelta),
#                                   y=c(r[1],r[1],r[2],r[2]),
#                                       chromIndex=c[[i]]$chromIndex[1])
#     }
#   }
#   return(output)
# })
#
# # navigation plot
# output$plot_navigation <- renderPlot({
#   polys <- navigationPolygons()
#
#   # if no polygons, plot placeholder
#   if (length(polys$polygon)==0) {
#     graphics::par(mar=c(0.2,0.8,0.2,0.8))
#     plot(0, type='n',
#          xlim=c(0,1), ylim=c(0,1),
#          xaxs='i', yaxs='i', axes=FALSE)
#     text(0.5, 0.5,
#          '(Select plotting variables)',
#          cex=1.5)
#     return()
#   }
#
#   # produce empty plot
#   graphics::par(mar=c(0.2,0.8,0.2,0.8), xpd=NA)
#   k <- 0.8
#   ylim <- c(polys$y_min,
#             polys$y_max*(0.5+k) + polys$y_min*(0.5-k))
#   plot(0, type='n',
#        xlim=c(polys$x_min,polys$x_max), ylim=ylim,
#        xaxs='i', yaxs='i', axes=FALSE)
#
#   # add text numbering chromosomes
#   if (!is.null(polys$chromLevels)) {
#     text(x=polys$chromMidpoint,
#          y=polys$y_max*(0.5+0.7) + polys$y_min*(0.5-0.7),
#          labels=polys$chromLevels, font=2)
#   }
#
#   # add polygons, including outside border
#   for (i in 1:length(polys$polygon)) {
#     polygon(polys$polygon[[i]]$x,
#             polys$polygon[[i]]$y,
#             col=c('black','red')[2-polys$polygon[[i]]$chromIndex%%2],
#             border='white')
#   }
#   polygon(c(polys$x_min,polys$x_max, polys$x_max,polys$x_min),
#           c(polys$y_min,polys$y_min, polys$y_max,polys$y_max))
#
#   # grey-out side regions based on slider
#   s <- input$slider_navigate
#   polygon(c(polys$x_min,s[1],s[1],polys$x_min),
#           c(polys$y_min,polys$y_min,polys$y_max,polys$y_max),
#           col=rgb(1,1,1,0.8))
#   polygon(c(polys$x_max,s[2],s[2],polys$x_max),
#           c(polys$y_min,polys$y_min,polys$y_max,polys$y_max),
#           col=rgb(1,1,1,0.8))
# })
#
#
# ###########################
# ## Box: Univariate Plots ##
# ###########################
#
# # box for plotting univariate distributions
# output$box_plot_linearManhattan <- shiny::renderUI({
#   shinydashboard::box(title=NULL, status="warning",
#       solidHeader=FALSE, collapsible=FALSE, width=12,
#       h2(input$univariate_main_title, align='center'),
#       h3(input$univariate_sub_title, align='center'),
#       plotOutput('plot1',height=200*ManhattanData()$y_col+100+20)
#   )
# })
#
# # plot univariate distributions
# output$plot1 <- renderPlot({
#
#   barplot(rep(10, 5), col=funky(5))

#   # only evaluate when apply_navigate_button is pressed
#   apply_navigate()
#
#   # isolate data used in plotting
#   outlierData_sub <- isolate(outlierData_sub())
#
#   # if outlierData_sub$x is NULL, return NULL
#   if (is.null(outlierData_sub$x))
#     return(NULL)
#
#   # isolate slider info
#   xlim <- isolate(c(input$slider_navigate[1],
#                     input$slider_navigate[2]))
#
#   # plot chromosomes in alternating black and red
#   if (is.null(outlierData_sub$chromIndex)) {
#     colVec <- 'black'
#   } else {
#     colVec <- c('black','red')[2-outlierData_sub$chromIndex%%2]
#   }
#
#   # work out which plots should have logged axes
#   k <- 1#outlierData_sub$y_cols
#   axis_log <- NULL
#   for (i in 1:k) {
#     axis_log <- c(axis_log,
#                   eval(parse(text=paste('input$check_log_univariate_',i,sep=''))))
#   }
#
#   # produce univariate plots
#   mar_top <- 20/(200*k+100+20)
#   mar_bot <- 100/(200*k+100+20)
#   mid <- 1-mar_top-mar_bot
#   for (i in 1:k) {
#
#     # get x and y values
#     x <- outlierData_sub$x+outlierData_sub$x_modifier
#     y <- outlierData_sub$y[,i]
#
#     # calculate figure position
#     plot_top <- mar_bot + mid*(k-i+1)/k
#     plot_bot <- mar_bot + mid*(k-i)/k
#     graphics::par(fig=c(0,1,plot_bot,plot_top),
#         mar=c(0,4.1,0,2.1),
#         new=i!=1)
#
#     # work out y-axis log action, and plot text error if needed
#     log_action <- ''
#     if (axis_log[i]) {
#       if (any(y<=0,na.rm=TRUE)) {
#         plot(0,type='n',
#              xlim=c(0,1),ylim=c(0,1),
#              xaxt='n',yaxt='n',ann=FALSE)
#         text(0.5,0.5,
#              labels='(cannot log y-axis due to negative or zero values)',
#              cex=1.5)
#         next()
#       } else {
#         log_action <- 'y'
#       }
#     }
#
#     # plot empty frame
#     plot(x, y, type='n',
#          log=log_action,
#          xaxs='i', xlim=xlim,
#          xaxt=ifelse(i==k,'l','n'))
#
#     #u <- graphics::par("usr")
#     #polygon(c(u[1],u[2],u[2],u[1]), c(u[3],u[3],u[4],u[4]), col=grey(1))
#
#     # add grid
#     tk1 <- axTicks(1)
#     tk2 <- axTicks(2)
#     abline(v=tk1, h=tk2, col=grey(0.95), lwd=1)
#
#     # finally add points and redraw frame
#     points(x, y, col=colVec, pch=20, cex=1)
#     shinydashboard::box()
#   }
#
# })



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
# df1 %>% ggvis(~x, ~y) %>%
#   layer_points(size:=5) %>%
#   layer_paths(data=df2,
#               x=~x,y=~y,
#               strokeWidth:=input_slider(1,10)) %>%
#   bind_shiny('p','p_ui')

#### using brushes
#plotOutput('plot_navigation',height=100,
#           brush=brushOpts(
#             id='navigation_brush',
#             direction='x'
#           )
#),
