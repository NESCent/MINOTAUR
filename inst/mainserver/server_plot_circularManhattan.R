
##################################
## CIRCULAR MANHATTAN PLOT PAGE ##  ------------------------------------------------------------------------------------
##################################


## generate reactiveValues lists for all initial values

## variables
rv_circularManhattan_button <- reactiveValues()
rv_circularManhattan_button <- 1 # 0
rv_circularManhattan_xaxis <- reactiveValues()
rv_circularManhattan_yaxis <- reactiveValues()
rv_circularManhattan_logx <- reactiveValues()
rv_circularManhattan_logy <- reactiveValues()
# rv_circularManhattan_flipx <- reactiveValues()
# rv_circularManhattan_flipy <- reactiveValues()

rv_circularManhattan_outlier.var1 <- reactiveValues()
rv_circularManhattan_outlier.cutoff1 <- reactiveValues()
rv_circularManhattan_outlier.tail1 <- reactiveValues()

rv_circularManhattan_outlier.var2 <- reactiveValues()
rv_circularManhattan_outlier.cutoff2 <- reactiveValues()
rv_circularManhattan_outlier.tail2 <- reactiveValues()

## aesthetics
# rv_circularManhattan_col.pal <- reactiveValues()
# rv_circularManhattan_n.bins <- reactiveValues()
# rv_circularManhattan_grid <- reactiveValues()

rv_circularManhattan_outlier.col.bg1 <- reactiveValues()
# rv_circularManhattan_outlier.col1 <- reactiveValues()
# rv_circularManhattan_outlier.pch1 <- reactiveValues()
rv_circularManhattan_outlier.transp1 <- reactiveValues()
rv_circularManhattan_outlier.cex1 <- reactiveValues()

rv_circularManhattan_outlier.col.bg2 <- reactiveValues()
# rv_circularManhattan_outlier.col2 <- reactiveValues()
# rv_circularManhattan_outlier.pch2 <- reactiveValues()
rv_circularManhattan_outlier.transp2 <- reactiveValues()
rv_circularManhattan_outlier.cex2 <- reactiveValues()






###########################################
## .set.reactiveValues.circularManhattan ##
###########################################
## fn to set reactiveValues initially for each k:
.set.reactiveValues.circularManhattan <- function(dat, k){

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

    #     dat <- HumanGWAS
    #     numCols <- which(sapply(c(1:ncol(dat[,c(2,4:ncol(dat))])),
    #                             function(e) is.numeric(dat[,c(2,4:ncol(dat))][,e])))
    #     var.choices <- c(names(dat)[numCols])

    var.choices <- c(names(dat$y)[numCols])

    x.var.choices <- y.var.choices <- o.var.choices <- var.choices

    #     x.var.sel <- x.var.choices[1]
    #     y.var.sel <- y.var.choices[2]
    #
    #     o.var.sel1 <- o.var.choices[3]
    #     o.var.sel2 <- o.var.choices[4]

    x.var.sel <- x.var.choices[1]
    y.var.sel <- y.var.choices[3]

    o.var.sel1 <- o.var.choices[2]
    o.var.sel2 <- o.var.choices[4]

    ## set intial values
    rv_circularManhattan_xaxis[[k]] <- x.var.sel
    rv_circularManhattan_yaxis[[k]] <- y.var.sel
    rv_circularManhattan_logx[[k]] <- "none"
    rv_circularManhattan_logy[[k]] <- "none"
    # rv_circularManhattan_flipx[[k]] <- "No"
    # rv_circularManhattan_flipy[[k]] <- "No"

    rv_circularManhattan_outlier.var1[[k]] <- o.var.sel1
    rv_circularManhattan_outlier.cutoff1[[k]] <- 0.002 # 0.05
    rv_circularManhattan_outlier.tail1[[k]] <- "Lower"

    rv_circularManhattan_outlier.var2[[k]] <- o.var.sel2
    rv_circularManhattan_outlier.cutoff2[[k]] <- 0.002 # 0.05
    rv_circularManhattan_outlier.tail2[[k]] <- "Lower"

    #     rv_circularManhattan_col.pal[[k]] <- "heat.colors"
    #     rv_circularManhattan_n.bins[[k]] <- 100
    #     rv_circularManhattan_grid[[k]] <- FALSE

    rv_circularManhattan_outlier.col.bg1[[k]] <- "red"
    # rv_circularManhattan_outlier.col1[[k]] <- "red"
    # rv_circularManhattan_outlier.pch1[[k]] <- "24"
    rv_circularManhattan_outlier.transp1[[k]] <- 0.5
    rv_circularManhattan_outlier.cex1[[k]] <- 1

    rv_circularManhattan_outlier.col.bg2[[k]] <- "blue"
    # rv_circularManhattan_outlier.col2[[k]] <- "blue"
    # rv_circularManhattan_outlier.pch2[[k]] <- "24"
    rv_circularManhattan_outlier.transp2[[k]] <- 0.5
    rv_circularManhattan_outlier.cex2[[k]] <- 1

  }
} # end .set.reactiveValues.circularManhattan



##############################################
## .update.reactiveValues.circularManhattan ##
##############################################
## fn to set reactiveValues initially for each k:
.update.reactiveValues.circularManhattan <- function(dat, k){

  k <- as.character(k)

  x.var.choices <- x.var.sel <-
    y.var.choices <- y.var.sel <-
    o.var.choices <- o.var.sel1 <- o.var.sel2 <- NULL

  ## get variables
  if(!is.null(dat)){

    ## Get currently-selected values:

    ## Get x-axis & y-axis
    xSelection <- eval(parse(text=paste("input$circularManhattan_xaxis", k, sep="_")))
    ySelection <- eval(parse(text=paste("input$circularManhattan_yaxis", k, sep="_")))

    logx <- eval(parse(text=paste("input$circularManhattan_logx", k, sep="_")))
    logy <- eval(parse(text=paste("input$circularManhattan_logy", k, sep="_")))
    # flipX <- eval(parse(text=paste("input$circularManhattan_flipx", k, sep="_")))
    # flipY <- eval(parse(text=paste("input$circularManhattan_flipy", k, sep="_")))

    ## Get plot aesthetics
    # col.pal <- eval(parse(text=paste("input$circularManhattan_col.pal", k, sep="_")))
    # n.bins <- eval(parse(text=paste("input$circularManhattan_n.bins", k, sep="_")))
    # grid <- eval(parse(text=paste("input$circularManhattan_grid", k, sep="_")))

    outlier.col.bg1 <- eval(parse(text=paste("input$circularManhattan_outlier.col.bg1", k, sep="_")))
    # outlier.col1 <- eval(parse(text=paste("input$circularManhattan_outlier.col1", k, sep="_")))
    outlier.transp1 <- eval(parse(text=paste("input$circularManhattan_outlier.transp1", k, sep="_")))
    # outlier.pch1 <- eval(parse(text=paste("input$circularManhattan_outlier.pch1", k, sep="_")))
    outlier.cex1 <- eval(parse(text=paste("input$circularManhattan_outlier.cex1", k, sep="_")))

    outlier.col.bg2 <- eval(parse(text=paste("input$circularManhattan_outlier.col.bg2", k, sep="_")))
    # outlier.col2 <- eval(parse(text=paste("input$circularManhattan_outlier.col2", k, sep="_")))
    outlier.transp2 <- eval(parse(text=paste("input$circularManhattan_outlier.transp2", k, sep="_")))
    # outlier.pch2 <- eval(parse(text=paste("input$circularManhattan_outlier.pch2", k, sep="_")))
    outlier.cex2 <- eval(parse(text=paste("input$circularManhattan_outlier.cex2", k, sep="_")))



    ## Get outlier var #1
    outlier.var1 <- eval(parse(text=paste("input$circularManhattan_outlier.var1", k, sep="_")))
    cutoff1 <- eval(parse(text=paste("input$circularManhattan_outlier.cutoff1", k, sep="_")))
    tail1 <- eval(parse(text=paste("input$circularManhattan_outlier.tail1", k, sep="_")))

    ## Get outlier var #2
    outlier.var2 <- eval(parse(text=paste("input$circularManhattan_outlier.var2", k, sep="_")))
    cutoff2 <- eval(parse(text=paste("input$circularManhattan_outlier.cutoff2", k, sep="_")))
    tail2 <- eval(parse(text=paste("input$circularManhattan_outlier.tail2", k, sep="_")))


    ## update "intial" values to current values
    rv_circularManhattan_xaxis[[k]] <- xSelection
    rv_circularManhattan_yaxis[[k]] <- ySelection
    rv_circularManhattan_logx[[k]] <- logx
    rv_circularManhattan_logy[[k]] <- logy
    # rv_circularManhattan_flipx[[k]] <- flipX
    # rv_circularManhattan_flipy[[k]] <- flipY

    rv_circularManhattan_outlier.var1[[k]] <- outlier.var1
    rv_circularManhattan_outlier.cutoff1[[k]] <- cutoff1
    rv_circularManhattan_outlier.tail1[[k]] <- tail1

    rv_circularManhattan_outlier.var2[[k]] <- outlier.var2
    rv_circularManhattan_outlier.cutoff2[[k]] <- cutoff2
    rv_circularManhattan_outlier.tail2[[k]] <- tail2

    # rv_circularManhattan_n.bins[[k]] <- n.bins
    # rv_circularManhattan_col.pal[[k]] <- col.pal
    # rv_circularManhattan_grid[[k]] <- grid

    rv_circularManhattan_outlier.col.bg1[[k]] <- outlier.col.bg1
    # rv_circularManhattan_outlier.col1[[k]] <- outlier.col1
    # rv_circularManhattan_outlier.pch1[[k]] <- outlier.pch1
    rv_circularManhattan_outlier.transp1[[k]] <- outlier.transp1
    rv_circularManhattan_outlier.cex1[[k]] <- outlier.cex1

    rv_circularManhattan_outlier.col.bg2[[k]] <- outlier.col.bg2
    # rv_circularManhattan_outlier.col2[[k]] <- outlier.col2
    # rv_circularManhattan_outlier.pch2[[k]] <- outlier.pch2
    rv_circularManhattan_outlier.transp2[[k]] <- outlier.transp2
    rv_circularManhattan_outlier.cex2[[k]] <- outlier.cex2

  }
} # end .update.reactiveValues.circularManhattan



## update K & set reactiveValues[[k]] if button pressed
observe({

  k <- input$new_circularManhattan_button

  if(length(k) == 1){
    k <- k[1]+1
    ## if input button updates, set new panel of initial input values

    dat <- data_outliers()

    ## if K updates:
    if(!is.null(dat)){

      if(k == 1){
        .set.reactiveValues.circularManhattan(dat, k)
      }else{
        if(k > rv_circularManhattan_button){
          ## update rv_circularManhattan_button
          rv_circularManhattan_button <- k

          # set reactive values for Kth element of rv lists
          .set.reactiveValues.circularManhattan(dat, k)
          # .update.reactiveValues.circularManhattan(dat, k)

          ## if more than one panel requested, update "initial" values for plots 1:k-1
          if(k > 1){
            for(i in 1:(k-1)){
              .update.reactiveValues.circularManhattan(dat, i)
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
output$box_circularManhattan <- renderUI({

  k <- 1
  k <- input$new_circularManhattan_button[1] + 1

  if(length(k) > 0){
    if(k > 0){
      lapply(1:k,function(i){

        dat <- title.k <- NULL

        ## get title
        title.k <- paste("Circular Manhattan Plot #", i, sep = " ")

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
                       .get.circularManhattan.controls(dat, i)
                ),

                column(8,
                       .get.circularManhattan.plot(dat, i),
                       .get.circularManhattan.controls.aes(dat, i)
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

.get.circularManhattan.controls <- function(dat, k=1){

  k <- as.character(k)

  out <- NULL

  ## get variables
  if(!is.null(dat)){

    # x.var.choices <-  c("Position", "Chromosome") # c(names(dat$y$pos), names(dat$y$chrom))

    ## get numeric variables
    numCols <- which(sapply(c(1:ncol(dat$y)),
                            function(e) is.numeric(dat$y[,e])))

    var.choices <- c(names(dat$y)[numCols])

    x.var.choices <- y.var.choices <- o.var.choices <- var.choices

  }

  ## get id's | k
  # k <- 1
  id_circularManhattan_xaxis <- paste("circularManhattan_xaxis", k, sep="_")
  id_circularManhattan_logx <- paste("circularManhattan_logx", k, sep="_")
  # id_circularManhattan_flipx <- paste("circularManhattan_flipx", k, sep="_")
  id_circularManhattan_yaxis <- paste("circularManhattan_yaxis", k, sep="_")
  id_circularManhattan_logy <- paste("circularManhattan_logy", k, sep="_")
  # id_circularManhattan_flipy <- paste("circularManhattan_flipy", k, sep="_")

  id_circularManhattan_outlier.var1 <- paste("circularManhattan_outlier.var1", k, sep="_")
  id_circularManhattan_outlier.cutoff1 <- paste("circularManhattan_outlier.cutoff1", k, sep="_")
  id_circularManhattan_outlier.tail1 <- paste("circularManhattan_outlier.tail1", k, sep="_")

  id_circularManhattan_outlier.var2 <- paste("circularManhattan_outlier.var2", k, sep="_")
  id_circularManhattan_outlier.cutoff2 <- paste("circularManhattan_outlier.cutoff2", k, sep="_")
  id_circularManhattan_outlier.tail2 <- paste("circularManhattan_outlier.tail2", k, sep="_")

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

        box(title="Select inner circle:", # "Univariate Distributions"
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
            selectizeInput(id_circularManhattan_xaxis,
                           label = 'Inner circle:',
                           choices = x.var.choices,
                           selected = rv_circularManhattan_xaxis[[k]], # x.var.sel,
                           multiple = FALSE),

            ## log(x-axis) ?
            radioButtons(id_circularManhattan_logx,
                         label = "Log inner axis?",
                         choices = list("log2", "log10", "none"),
                         selected= rv_circularManhattan_logx[[k]], # "none",
                         inline=TRUE),

            #             ## Flip x-axis ?
            #             radioButtons(id_circularManhattan_flipx,
            #                          label = "Invert inner axis?",
            #                          choices = list("Yes", "No"),
            #                          selected= rv_circularManhattan_flipx[[k]], # "No",
            #                          inline=TRUE),

            style = list('background-color: #FFECB3') # pale amber
            ),

        ###################
        ## Choose y-axis ##
        ###################

        box(title="Select outer circle:",
            # status="info",
            # status = "primary",
            status="warning",
            solidHeader=TRUE,
            collapsible=TRUE,
            width=12,

            ## Choose y-axis variable
            selectizeInput(id_circularManhattan_yaxis,
                           label = 'Outer circle:',
                           choices = y.var.choices,
                           selected =  rv_circularManhattan_yaxis[[k]], # y.var.sel,
                           multiple = FALSE),

            ## log(y-axis) ?
            radioButtons(id_circularManhattan_logy,
                         label = "Log outer axis?",
                         choices = list("log2", "log10", "none"),
                         selected= rv_circularManhattan_logy[[k]], # "none",
                         inline=TRUE),

            #             ## Flip y-axis ?
            #             radioButtons(id_circularManhattan_flipy,
            #                          label = "Invert outer axis?",
            #                          choices = list("Yes", "No"),
            #                          selected= rv_circularManhattan_flipy[[k]], # "No",
            #                          inline=TRUE),

            style = list('background-color: #FFECB3') # pale amber

        ),


        ###############################################
        ## Choose outlier variable (usually p-value) ##
        ###############################################

        ## NOTE: I'm not 100% sure what the best way to refer to this variable is...
        ## ie. "Second variable" or "Outlier detection variable" or "Univariate outlier detection variable"??

        box(title="Select outlier variable #1:",
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
            selectizeInput(id_circularManhattan_outlier.var1,
                           label = NULL,
                           choices = o.var.choices,
                           selected = rv_circularManhattan_outlier.var1[[k]], # o.var.sel,
                           multiple = FALSE),

            ## Cut-off for outliers to overlay
            # eg 0.01
            textInput(id_circularManhattan_outlier.cutoff1,
                      label = "Cut-off for outliers to overlay",
                      value =  rv_circularManhattan_outlier.cutoff1[[k]] # 0.05
            ),

            radioButtons(id_circularManhattan_outlier.tail1,
                         label = "Tail",
                         choices = c("Lower", "Upper", "Two-tailed"),
                         selected =  rv_circularManhattan_outlier.tail1[[k]], # "Lower",
                         inline=TRUE),

            style = list('background-color: #FFECB3') # pale amber
            ),



        box(title="Select outlier variable #2:",
            # status="info",
            # status = "primary",
            status="warning",
            solidHeader=TRUE,
            collapsible=TRUE,
            width=12,

            ## Mark outliers by second variable (usually p-value)
            h5(strong('Highlight outliers by this variable:')),
            # p("For example, you may wish to identify outliers according to a p-value
              # that is recorded in another column of the data table."),
            selectizeInput(id_circularManhattan_outlier.var2,
                           label = NULL,
                           choices = o.var.choices,
                           selected = rv_circularManhattan_outlier.var2[[k]], # o.var.sel,
                           multiple = FALSE),

            ## Cut-off for outliers to overlay
            # eg 0.01
            textInput(id_circularManhattan_outlier.cutoff2,
                      label = "Cut-off for outliers to overlay",
                      value =  rv_circularManhattan_outlier.cutoff2[[k]] # 0.05
            ),

            radioButtons(id_circularManhattan_outlier.tail2,
                         label = "Tail",
                         choices = c("Lower", "Upper", "Two-tailed"),
                         selected =  rv_circularManhattan_outlier.tail2[[k]], # "Lower",
                         inline=TRUE),

            style = list('background-color: #FFECB3') # pale amber
        )


        )

  return(out)
} # end .get.circularManhattan.controls





#########################################
## .get.circularManhattan.controls.aes ##
#########################################
## fn to get widgets to control plot AESTHETICS under plot
.get.circularManhattan.controls.aes <- function(dat, k=1){

  k <- as.character(k)

  ## get Id's | k
  # id_circularManhattan_col.pal <- paste("circularManhattan_col.pal", k, sep="_")
  # id_circularManhattan_n.bins <- paste("circularManhattan_n.bins", k, sep="_")
  # id_circularManhattan_grid <- paste("circularManhattan_grid", k, sep="_")

  id_circularManhattan_outlier.col.bg1 <- paste("circularManhattan_outlier.col.bg1", k, sep="_")
  # id_circularManhattan_outlier.col1 <- paste("circularManhattan_outlier.col1", k, sep="_")
  # id_circularManhattan_outlier.pch1 <- paste("circularManhattan_outlier.pch1", k, sep="_")
  id_circularManhattan_outlier.transp1 <- paste("circularManhattan_outlier.transp1", k, sep="_")
  id_circularManhattan_outlier.cex1 <- paste("circularManhattan_outlier.cex1", k, sep="_")

  id_circularManhattan_outlier.col.bg2 <- paste("circularManhattan_outlier.col.bg2", k, sep="_")
  # id_circularManhattan_outlier.col2 <- paste("circularManhattan_outlier.col2", k, sep="_")
  # id_circularManhattan_outlier.pch2 <- paste("circularManhattan_outlier.pch2", k, sep="_")
  id_circularManhattan_outlier.transp2 <- paste("circularManhattan_outlier.transp2", k, sep="_")
  id_circularManhattan_outlier.cex2 <- paste("circularManhattan_outlier.cex2", k, sep="_")


  out <- NULL

  out <-
    box(title="Adjust Plot Aesthetics:",
        status="warning",
        solidHeader=FALSE,
        collapsible=TRUE,
        width=12,

        box(title="Outlier #1 aesthetics:",
            status="warning",
            solidHeader=TRUE,
            collapsible=TRUE,
            width=12,

            fluidRow(
              column(4,
            selectizeInput(id_circularManhattan_outlier.col.bg1,
                           label = "Outlier colour (fill):",
                           choices = list("Red" = "red",
                                          "Orange" = "orange",
                                          "Yellow" = "yellow",
                                          "Green" = "green",
                                          "Blue" = "blue",
                                          "Purple" = "purple"),
                           selected =  rv_circularManhattan_outlier.col.bg1[[k]], # "blue",
                           multiple=FALSE)),

            column(4,
                   sliderInput(id_circularManhattan_outlier.transp1,
                               label = "Outlier transparency:",
                               min = 0, max = 1,
                               value =  rv_circularManhattan_outlier.transp1[[k]], # 0.25,
                               step = 0.05)),

            column(4,
                   sliderInput(id_circularManhattan_outlier.cex1,
                               label = "Outlier size:",
                               min = 0, max = 3,
                               value =  rv_circularManhattan_outlier.cex1[[k]], # 1.5,
                               step = 0.1))
            ),

            #             hr(),
            #
            #             fluidRow(
            #               column(6,
            #                      selectizeInput(id_circularManhattan_outlier.pch1,
            #                                     label = "Outlier shape:",
            #                                     choices = list("Circle" = "21",
            #                                                    "Square" = "22",
            #                                                    "Diamond" = "23",
            #                                                    "Triangle, point-up" = "24",
            #                                                    "Triangle, point-down" = "25"
            #                                     ),
            #                                     selected =  rv_circularManhattan_outlier.pch1[[k]], # "24",
            #                                     multiple=FALSE)),
            #
            #             column(6,
            #                    selectizeInput(id_circularManhattan_outlier.col1,
            #                                   label = "Outlier colour (outline):",
            #                                   choices = list("Red" = "red",
            #                                                  "Orange" = "orange",
            #                                                  "Yellow" = "yellow",
            #                                                  "Green" = "green",
            #                                                  "Blue" = "blue",
            #                                                  "Purple" = "purple"),
            #                                   selected =  rv_circularManhattan_outlier.col1[[k]], # NULL,
            #                                   multiple=FALSE))
            #             ),

            style = list('background-color: #FFECB3') # pale amber
        ),


        box(title="Outlier #2 aesthetics:",
            status="warning",
            solidHeader=TRUE,
            collapsible=TRUE,
            width=12,

            fluidRow(
              column(4,
                     selectizeInput(id_circularManhattan_outlier.col.bg2,
                                    label = "Outlier colour (fill):",
                                    choices = list("Red" = "red",
                                                   "Orange" = "orange",
                                                   "Yellow" = "yellow",
                                                   "Green" = "green",
                                                   "Blue" = "blue",
                                                   "Purple" = "purple"),
                                    selected =  rv_circularManhattan_outlier.col.bg2[[k]], # "orange",
                                    multiple=FALSE)),

              column(4,
                     sliderInput(id_circularManhattan_outlier.cex2,
                                 label = "Outlier size:",
                                 min = 0, max = 3,
                                 value =  rv_circularManhattan_outlier.cex2[[k]], # 1.5,
                                 step = 0.1)),

              column(4,
                     sliderInput(id_circularManhattan_outlier.transp2,
                                 label = "Outlier transparency:",
                                 min = 0, max = 1,
                                 value =  rv_circularManhattan_outlier.transp2[[k]], # 0.25,
                                 step = 0.05))
            ),

            #             hr(),
            #
            #             fluidRow(
            #               column(6,
            #                      selectizeInput(id_circularManhattan_outlier.pch2,
            #                                     label = "Outlier shape:",
            #                                     choices = list("Circle" = "21",
            #                                                    "Square" = "22",
            #                                                    "Diamond" = "23",
            #                                                    "Triangle, point-up" = "24",
            #                                                    "Triangle, point-down" = "25"
            #                                     ),
            #                                     selected =  rv_circularManhattan_outlier.pch2[[k]], # "24",
            #                                     multiple=FALSE)),
            #
            #               column(6,
            #                      selectizeInput(id_circularManhattan_outlier.col2,
            #                                     label = "Outlier colour (outline):",
            #                                     choices = list("Red" = "red",
            #                                                    "Orange" = "orange",
            #                                                    "Yellow" = "yellow",
            #                                                    "Green" = "green",
            #                                                    "Blue" = "blue",
            #                                                    "Purple" = "purple"),
            #                                     selected =  rv_circularManhattan_outlier.col2[[k]], # NULL,
            #                                     multiple=FALSE))
            #             ),

            style = list('background-color: #FFECB3') # pale amber
        )

    ) # end box

  return(out)

} # end .get.circularManhattan.controls.aes



####################################
## BUTTON: Generate another plot? ##
####################################
output$box_circularManhattan_button <- renderUI({
  box(
    title = "Generate another plot?",
    solidHeader = TRUE,
    status = "primary",
    value = NULL,
    width=12,

    ## button
    actionButton(inputId = "new_circularManhattan_button",
                 label = "Yes, please!",
                 icon = icon("cog"))
  )
})




##############################
## get.circularManhattan.plot ##
##############################
.get.circularManhattan.plot <- function(dat, k=1){

  out <- NULL

  if(!is.null(k)){

    ## get unique outputId
    id_circularManhattan <- paste("id_circularManhattan", k, sep="_")

    out <-
      box(title=NULL,
          status="warning",
          solidHeader=FALSE,
          collapsible=TRUE,
          width=12,
          # plotOutput("plot_circularManhattan_plot")
          renderPlot(plotOutput(
            outputId = id_circularManhattan,
            .get.circularManhattan(input, k=k)))
      )
  }
  return(out)
}
# end .get.circularManhattan.plot

######################################################################################################################

#############################
## Circular Manhattan Plot ##
#############################


############################
## .get.circularManhattan ##
############################
.get.circularManhattan <- function(input, k=1){

  circularManhattan <- dat <- xData <- yData <- xSelection <- ySelection <- SNPselection <-
    logx <- logy <- flipX <- flipY <- col.pal <-
    outlier.var1 <- cutoff1 <- tail1 <-
    outlier.var2 <- cutoff2 <- tail2 <-
    outlier.col.bg1 <- outlier.col1 <- outlier.transp1 <- outlier.pch1 <- outlier.cex1 <-
    outlier.col.bg2 <- outlier.col2 <- outlier.transp2 <- outlier.pch2 <- outlier.cex2 <-
    n.bins <- NULL

  k <- as.character(k)

  ## Get x-axis & y-axis
  xSelection <- eval(parse(text=paste("input$circularManhattan_xaxis", k, sep="_")))
  ySelection <- eval(parse(text=paste("input$circularManhattan_yaxis", k, sep="_")))

  logx <- eval(parse(text=paste("input$circularManhattan_logx", k, sep="_")))
  logy <- eval(parse(text=paste("input$circularManhattan_logy", k, sep="_")))
  # flipX <- eval(parse(text=paste("input$circularManhattan_flipx", k, sep="_")))
  # flipY <- eval(parse(text=paste("input$circularManhattan_flipy", k, sep="_")))


  ## Get data and plot output
  if(!is.null(data_outliers())){
    if(!is.null(xSelection) & !is.null(ySelection)){

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
      #       if(flipX=="No"){flipX=1}else{
      #         if(flipX=="Yes"){flipX=-1}}
      #
      #       ## Invert y-axis?
      #       if(flipY=="No"){flipY=1}else{
      #         if(flipY=="Yes"){flipY=-1}}

      #########################
      ## Get plot aesthetics ##
      #########################

      ## Get plot aesthetics
      # col.pal <- eval(parse(text=paste("input$circularManhattan_col.pal", k, sep="_")))
      # n.bins <- eval(parse(text=paste("input$circularManhattan_n.bins", k, sep="_")))
      # grid <- eval(parse(text=paste("input$circularManhattan_grid", k, sep="_")))

      outlier.col.bg1 <- eval(parse(text=paste("input$circularManhattan_outlier.col.bg1", k, sep="_")))
      # outlier.col1 <- eval(parse(text=paste("input$circularManhattan_outlier.col1", k, sep="_")))
      outlier.transp1 <- eval(parse(text=paste("input$circularManhattan_outlier.transp1", k, sep="_")))
      # outlier.pch1 <- as.numeric(eval(parse(text=paste("input$circularManhattan_outlier.pch1", k, sep="_"))))
      outlier.cex1 <- eval(parse(text=paste("input$circularManhattan_outlier.cex1", k, sep="_")))

      outlier.col.bg2 <- eval(parse(text=paste("input$circularManhattan_outlier.col.bg2", k, sep="_")))
      # outlier.col2 <- eval(parse(text=paste("input$circularManhattan_outlier.col2", k, sep="_")))
      outlier.transp2 <- eval(parse(text=paste("input$circularManhattan_outlier.transp2", k, sep="_")))
      # outlier.pch2 <- as.numeric(eval(parse(text=paste("input$circularManhattan_outlier.pch2", k, sep="_"))))
      outlier.cex2 <- eval(parse(text=paste("input$circularManhattan_outlier.cex2", k, sep="_")))


      ## Get outlier var
      outlier.var1 <- eval(parse(text=paste("input$circularManhattan_outlier.var1", k, sep="_")))
      cutoff1 <- as.numeric(eval(parse(text=paste("input$circularManhattan_outlier.cutoff1", k, sep="_"))))
      tail1 <- eval(parse(text=paste("input$circularManhattan_outlier.tail1", k, sep="_")))

      outlier.var2 <- eval(parse(text=paste("input$circularManhattan_outlier.var2", k, sep="_")))
      cutoff2 <- as.numeric(eval(parse(text=paste("input$circularManhattan_outlier.cutoff2", k, sep="_"))))
      tail2 <- eval(parse(text=paste("input$circularManhattan_outlier.tail2", k, sep="_")))

      outlier.transp1 <- 1-outlier.transp1
      outlier.transp2 <- 1-outlier.transp2

      n <- 100
      start <- 0.25
      end <- 0.9
      alpha <- 1
      #       if(col.pal == "gray.colors"){
      #         col.pal <- eval(parse(text=paste(col.pal, "(n=n, start=start, end=end)", sep="")))
      #       }else{
      #         col.pal <- eval(parse(text=paste(col.pal, "(n=n, alpha=alpha)", sep="")))
      #       }


      ## Get X, Chr, Pos variables
      #       ## Get Pos
      #       Pos <- eval(parse(text="dat$pos"))
      #
      #       ## Get Chr
      #       Chr <- eval(parse(text="dat$chrom"))

      ## Get x-variable data
      ## ie. EITHER POS OR CHROMOSOME...
      if(!is.null(xSelection)){
        ## Get variable to plot
        xData <- eval(parse(text=paste("dat", xSelection, sep="$"))) # xSelection # dat[,Pos]
      }
      ## Get x-variable data
      ## ie. VARIABLE TO PLOT
      if(!is.null(ySelection)){
        ## Get variable to plot
        yData <- eval(parse(text=paste("dat$y", ySelection, sep="$")))
      }


      if(is.na(cutoff1)){cutoff1=0}
      if(tail2=="Upper"){
        cutoff2=(1-cutoff2)
      }

      if(is.na(cutoff2)){cutoff2=0}
      if(tail2=="Upper"){
        cutoff2=(1-cutoff2)
      }



      ## Get outlier-variable data
      #       if(!is.null(outlier.var1)){
      #         ## Get variable to plot
      #         outlier.Data1 <- eval(parse(text=paste("dat$y", outlier.var1, sep="$")))
      #       }
      #
      #       if(!is.null(outlier.var2)){
      #         ## Get variable to plot
      #         outlier.Data2 <- eval(parse(text=paste("dat$y", outlier.var2, sep="$")))
      #       }

      #       ## get log of x and y variables:
      #       toRemove <- toRemoveX <- toRemoveY <- NULL
      #       if(length(logx) == 1){
      #         toRemoveX <- which(xData <= 0)
      #       }
      #       if(length(logy) == 1){
      #         toRemoveY <- which(yData <= 0)
      #       }
      #       toRemove <- c(toRemoveX, toRemoveY)
      #
      #       xData <- replace(xData, toRemove, NA)
      #       yData <- replace(yData, toRemove, NA)
      #
      #       if(length(logx)==1){xData=log(xData+1e-40, logx)}
      #       if(length(logy)==1){yData=log(yData+1e-40, logy)}


      #       outlier.Data1NoNA <- outlier.Data1[!is.na(outlier.Data1)]
      #       outlier.Data1New <- rank(outlier.Data1NoNA)/length(outlier.Data1NoNA)
      #       outlier.Data1New2 <- outlier.Data1
      #       outlier.Data1New2[!is.na(outlier.Data1)] <- outlier.Data1New
      #
      #       if(tail1=="Lower"){
      #         xData_sub <- xData[outlier.Data1New2<=cutoff1]
      #         yData_sub <- yData[outlier.Data1New2<=cutoff1]
      #       }
      #       if(tail1=="Upper"){
      #         xData_sub <- xData[outlier.Data1New2>=cutoff1]
      #         yData_sub <- yData[outlier.Data1New2>=cutoff1]
      #       }
      #       if(tail1=="Two-tailed"){
      #         xData_sub_l <- xData[outlier.Data1New2<=cutoff1]
      #         yData_sub_l <- yData[outlier.Data1New2<=cutoff1]
      #
      #         cutoff1 <- (1-cutoff1)
      #         xData_sub_u <- xData[outlier.Data1New2>=cutoff1]
      #         yData_sub_u <- yData[outlier.Data1New2>=cutoff1]
      #
      #         xData_sub <- c(xData_sub_l, xData_sub_u)
      #         yData_sub <- c(yData_sub_l, yData_sub_u)
      #       }
      #
      #       xData <- xData*flipX
      #       yData <- yData*flipY
      #       xData_sub <- xData_sub*flipX
      #       yData_sub <- yData_sub*flipY

      # get colors
      #          get.levels <- levels(as.factor(colData))
      #          n.levels <- length(get.levels)
      #          colIndex <- as.numeric(as.factor(colData))
      #           if(!(colPal=="black")){
      #            myCol <- get(colPal)(n.levels)[colIndex]
      #            }else(myCol <- rgb(0,0,0,0.2))


      ## PRODUCE PLOT
      circularManhattan <- .circosmht(mydata=dat,
                                      traitsname = c(xSelection, ySelection),
                                      logV1 = logx, logV2 = logy,
                                      trait.pvalnam = c(outlier.var1, outlier.var2), # c("Trait1_P","Trait2_P"), # c(xSelection, ySelection), ## ????
                                      pcut.outlier= c(cutoff1, cutoff2),
                                      outlier.col = c(outlier.col.bg1, outlier.col.bg2),
                                      outlier.transp = c(outlier.transp1, outlier.transp2),
                                      outlier.cex = c(outlier.cex1, outlier.cex2))


      }
    }

  #   circularManhattan <-   .circosmht(mydata=HumanGWAS,
  #                                     traitsname = c("Trait1_Beta","Trait2_Beta"),
  #                                     logV1 = NULL, logV2 = NULL,
  #                                     trait.pvalnam = c("Trait1_P","Trait2_P"),
  #                                     pcut.outlier=0.002)

  # circularManhattan
  return(circularManhattan)

  } # end .get.circularManhattan




#########################################################################################################################################



################
## .linkcreat ##
################
.linkcreat <- function(dat = seg.value, traitid = NULL,
                       pvalid = NULL, pcut.outlier=0.002){
  ## a function for create link line for each trait
  #dat = seg.value; traitid = 3; pvalid = 4;pcut.outlier=0.001
  dat.outlier = dat[which(dat[, pvalid] < pcut.outlier),c("seg.name","seg.no")]

  #   snpbp.pair <- t(combn(dat.outlier[,2],2))
  #   snpchr.pair <- t(combn(dat.outlier[,1],2))

  if(length(dat.outlier$seg.no) > 1){
    snpbp.pair <- t(combn(dat.outlier$seg.no, 2))
  }else{
    snpbp.pair <- transform(dat.outlier, seg.no=seg.no)
  }

  if(length(dat.outlier$seg.name) > 1){
    snpchr.pair <- t(combn(dat.outlier$seg.name, 2))
  }else{
    snpchr.pair <- transform(dat.outlier, seg.name=seg.name)
  }

  link.outlier.v <- data.frame(seg1 = snpchr.pair[,1],
                               pos1=snpbp.pair[,1],
                               name1=paste("n", 1:dim(snpchr.pair)[1], sep=""),
                               seg2=snpchr.pair[,2], pos2=snpbp.pair[,2])
  return(link.outlier.v)
} # end .linkcreat




################
## .circosmht ##
################
.circosmht <- function(mydata=mytoys,
                       traitsname = c("Trait1_Beta","Trait2_Beta"),
                       logV1 = NULL, logV2 = NULL,
                       trait.pvalnam = c("Trait1_P","Trait2_P"),
                       pcut.outlier=c(0.002, 0.002),
                       outlier.col = c("red", "blue"),
                       outlier.transp = c(0.5, 0.5),
                       outlier.cex = c(1,1)){

  seg.file <- data.frame(seg.name=mydata$chrom,
                         seg.Start=mydata$pos,
                         seg.End=mydata$pos+1,
                         the.v="NA", NO="NA")

  #   data("HumanGWAS")
  #   mydata <- HumanGWAS
  #   traitsname = c("Trait1_Beta","Trait2_Beta")
  #   logV1 = NULL
  #   logV2 = NULL
  #   trait.pvalnam = c("Trait1_P","Trait2_P")
  #   pcut.outlier=0.002
  #
  #   seg.file <- data.frame(seg.name=mydata$Chr,
  #                          seg.Start=mydata$BP,
  #                          seg.End=mydata$BP+1,
  #                          the.v="NA", NO="NA")
  #   seg.value = subset(mydata, select=-SNP)
  #   # seg.value <- mydata
  #   names(seg.value) <- c("Chromosome", "Position", names(seg.value)[c(3:length(names(seg.value)))])
  #   toKeep <- which(names(seg.value) %in% c("Chromosome", "Position", traitsname, trait.pvalnam))
  #   seg.value <- seg.value[,toKeep]
  #   mydata <- seg.value


  toKeep <- which(names(mydata$y) %in% c(traitsname, trait.pvalnam))
  seg.value <- cbind(mydata$chrom, mydata$pos, mydata$y[,toKeep])
  names(seg.value) <- c("Chromosome", "Position", names(mydata$y[,toKeep]))
  mydata <- seg.value

  # print("mydata"); print(str(mydata))

  # print(str(seg.value))
  # print("SUMMARY CHROMOSOME"); print(summary(seg.value$Chromosome))

  traitidxlist = match(traitsname, names(seg.value))
  trait.pidxlist = match(trait.pvalnam, names(seg.value))
  chridx = match("Chromosome", names(seg.value))
  BPidx = match("Position", names(seg.value))


  if(is.null(logV1)){
    seg.value[,traitidxlist[1]] = seg.value[,traitidxlist[1]]
  } else{
    if(logV1 %in% c("log2", "log10")){
      if(length(which(seg.value[,traitidxlist[1]] < 0)) > 0) {
        stop("Selected Y-axis variable contains negative values, can't be log-transformed\n ")}
      logbase = ifelse(logV1 %in% "log2",2, 10)
      seg.value[,traitidxlist[1]] = -log(abs(seg.value[,traitidxlist[1]]), logbase)
    }
  }

  if(is.null(logV2)){
    seg.value[,traitidxlist[2]] = seg.value[,traitidxlist[2]]
  } else{
    if(logV2 %in% c("log2", "log10")){
      if(length(which(seg.value[,traitidxlist[2]] < 0)) > 0) {
        stop("Selected Y-axis variable contains negative values, can't be log-transformed\n ")}
      logbase = ifelse(logV2 %in% "log2",2, 10)
      seg.value[,traitidxlist[2]] = -log(abs(seg.value[,traitidxlist[2]]), logbase)
    }
  }

  names(seg.value)[chridx] <- "seg.name"
  names(seg.value)[BPidx] <- "seg.no"

  # print("seg.value"); print(str(seg.value))

  seg.number <- length(unique(mydata$Chromosome))
  seg.name <- as.character(sort(unique(mydata$Chromosome))) # paste("chr", , sep=)

  db <- segAnglePo(seg.file, seg=seg.name);

  colors <- brewer.pal(9, "Set1")
  # barplot(rep(10, length(colors)), col=colors) ## temp: check out colors

  # print("traitidxlist"); print(str(traitidxlist))

  o.l <- list()
  for(i in 1:length(traitidxlist)){
  o.l[[i]] = .linkcreat(dat=seg.value,
                        traitid=traitidxlist[i],
                        pvalid=trait.pidxlist[i],
                        pcut.outlier=pcut.outlier[i])
  # print("OUTLIER.LINK"); print(str(o.l[[i]]))
  }

  # outlier.link <- o.l[[1]]

  par(mar=c(2,2,2,2));
  par(cex.axis=1, cex.lab=1, cex.main=1.2, cex.sub=1);
  plot(c(1,800),c(1,800),type="n",axes=F,xlab="",ylab="",main="");
  ## get outer-most lines indicating chromosomes:
  circos(R=400,type="chr", cir=db,
         col=rep(alpha(colors,0.6), length.out=seg.number),
         print.chr.lab=T,
         W=40, scale=T); #scale=T

  for (i in 1:length(traitidxlist)){
    # tmpcolor = alpha(colors[i], 0.3)
    tmpcolor = alpha(outlier.col[i], outlier.transp[i])
    # outlier.link = outlier.link
    outlier.link = o.l[[i]]
    ## get dots:
    circos(R= 120 + (i-1) * 100, cir=db,
           W= 180, mapping=seg.value,
           col.v=traitidxlist[i],type="s",
           B=F, col=tmpcolor,
           cex=outlier.cex[i],
           lwd=0.15, scale=T) #scale=T
    ## get lines indicating links:
    circos(R=100, cir=db, W=100,
           mapping=outlier.link,
           type="link",lwd=0.2,
           col= tmpcolor)
  }

} # end .circosmht

#####
# > str(seg.value)

## GOOD: ##

# 'data.frame':	17624 obs. of  6 variables:
#   $ seg.name   : int  1 1 1 1 1 1 1 1 1 1 ...
# $ seg.no     : int  10 155313 276777 310011 319847 324206 335029 696942 706680 709966 ...
# $ Trait1_Beta: num  -2.609 0.115 1.398 0.683 1.119 ...
# $ Trait1_P   : num  0.3197 0.9059 0.5947 0.2017 0.0899 ...
# $ Trait2_Beta: num  0.673 -3.054 6.557 -0.602 0.35 ...
# $ Trait2_P   : num  0.8509 0.185 0.0661 0.5467 0.7809 ...

## BAD (no -SNP): ##
# 'data.frame':	17624 obs. of  6 variables:
#   $ seg.name   : int  1 1 1 1 1 1 1 1 1 1 ...
# $ seg.no     : Factor w/ 17624 levels "AFFX-SNP_10783944__rs7287369",..: 508 910 7913 15526 5264 8979 13252 3508 13991 11452 ...
# $ Trait1_Beta: num  -2.609 0.115 1.398 0.683 1.119 ...
# $ Trait1_P   : num  0.3197 0.9059 0.5947 0.2017 0.0899 ...
# $ Trait2_Beta: num  0.673 -3.054 6.557 -0.602 0.35 ...
# $ Trait2_P   : num  0.8509 0.185 0.0661 0.5467 0.7809 ...




# ################
# ## .circosmht ##
# ################
# # WORKING EXAMPLE on HumanGWAS (requires no inputs)
# .circosmht <- function(mydata=HumanGWAS,
#                        traitsname = c("Trait1_Beta","Trait2_Beta"),
#                        logV1 = NULL, logV2 = NULL,
#                        trait.pvalnam = c("Trait1_P","Trait2_P"),
#                        pcut.outlier=0.002){
#
#   require(OmicCircos)
#
#   #   seg.file <- data.frame(seg.name=mydata$chrom,
#   #                          seg.Start=mydata$pos,
#   #                          seg.End=mydata$pos+1,
#   #                          the.v="NA", NO="NA")
#
#   data("HumanGWAS")
#   mydata <- HumanGWAS
#   traitsname = c("Trait1_Beta","Trait2_Beta")
#   logV1 = NULL
#   logV2 = NULL
#   trait.pvalnam = c("Trait1_P","Trait2_P")
#   pcut.outlier=0.002
#
#   seg.file <- data.frame(seg.name=mydata$Chr,
#                          seg.Start=mydata$BP,
#                          seg.End=mydata$BP+1,
#                          the.v="NA", NO="NA")
#   seg.value = subset(mydata, select=-SNP)
#   # seg.value <- mydata
#   names(seg.value) <- c("Chromosome", "Position", names(seg.value)[c(3:length(names(seg.value)))])
#   toKeep <- which(names(seg.value) %in% c("Position", "Chromosome", traitsname, trait.pvalnam))
#   seg.value <- seg.value[,toKeep]
#   mydata <- seg.value
#
#
#   #   # seg.value = subset(mydata, select=-SNP)
#   #   # seg.value <- cbind(mydata$pos, mydata$chrom, mydata$y)
#   #   # names(seg.value) <- c("Position", "Chromosome", names(mydata$y))
#
#   #   toKeep <- which(names(mydata$y) %in% c(traitsname, trait.pvalnam))
#   #   seg.value <- cbind(mydata$pos, mydata$chrom, mydata$y[,toKeep])
#   #   names(seg.value) <- c("Position", "Chromosome", names(mydata$y[,toKeep]))
#   #   mydata <- seg.value
#
#   print("mydata"); print(str(mydata))
#
#   # print(str(seg.value))
#   print("SUMMARY CHROMOSOME"); print(summary(seg.value$Chromosome))
#
#   traitidxlist = match(traitsname, names(seg.value))
#   trait.pidxlist = match(trait.pvalnam, names(seg.value))
#   chridx = match("Chromosome",names(seg.value))
#   BPidx = match("Position", names(seg.value))
#
#
#   if(is.null(logV1)){
#     seg.value[,traitidxlist[1]] = seg.value[,traitidxlist[1]]
#   } else{
#     if(logV1 %in% c("log2", "log10")){
#       if(length(which(seg.value[,traitidxlist[1]] < 0)) > 0) {
#         stop("Selected Y-axis variable contains negative values, can't be log-transformed\n ")}
#       logbase = ifelse(logV1 %in% "log2",2, 10)
#       seg.value[,traitidxlist[1]] = -log(abs(seg.value[,traitidxlist[1]]), logbase)
#     }
#   }
#
#   if(is.null(logV2)){
#     seg.value[,traitidxlist[2]] = seg.value[,traitidxlist[2]]
#   } else{
#     if(logV2 %in% c("log2", "log10")){
#       if(length(which(seg.value[,traitidxlist[2]] < 0)) > 0) {
#         stop("Selected Y-axis variable contains negative values, can't be log-transformed\n ")}
#       logbase = ifelse(logV2 %in% "log2",2, 10)
#       seg.value[,traitidxlist[2]] = -log(abs(seg.value[,traitidxlist[2]]), logbase)
#     }
#   }
#
#   names(seg.value)[chridx] <- "seg.name"
#   names(seg.value)[BPidx] <- "seg.no"
#
#   print("seg.value"); print(str(seg.value))
#
#   seg.number <- length(unique(mydata$Chromosome))
#   seg.name <- as.character(sort(unique(mydata$Chromosome))) # paste("chr", , sep=)
#
#   db <- segAnglePo(seg.file, seg=seg.name);
#
#   colors <- brewer.pal(9, "Set1")
#
#   print("traitidxlist"); print(str(traitidxlist))
#
#   o.l <- list()
#   for(i in 1:length(traitidxlist)){
#     o.l[[i]] = .linkcreat(dat=seg.value,
#                           traitid=traitidxlist[i],
#                           pvalid=trait.pidxlist[i],
#                           pcut.outlier=pcut.outlier)
#     print("OUTLIER.LINK"); print(str(o.l[[i]]))
#   }
#
#   outlier.link <- o.l[[1]]
#
#   par(mar=c(2,2,2,2));
#   par(cex.axis=1, cex.lab=1, cex.main=1.2, cex.sub=1);
#   plot(c(1,800),c(1,800),type="n",axes=F,xlab="",ylab="",main="");
#   ## get outer-most lines indicating chromosomes:
#   circos(R=400,type="chr", cir=db,
#          col=rep(alpha(colors,0.6), length.out=seg.number),
#          print.chr.lab=T,
#          W=40, scale=T); #scale=T
#
#   for (i in 1:length(traitidxlist)){
#     tmpcolor = alpha(colors[i], 0.3)
#     outlier.link = outlier.link
#     # outlier.link = outlier.LINK[[i]];
#     ## get dots:
#     circos(R= 120 + (i-1) * 100, cir=db,
#            W= 180, mapping=seg.value,
#            col.v=traitidxlist[i],type="s",
#            B=F, col=tmpcolor,lwd=0.15, scale=T) #scale=T
#     ## get lines indicating links:
#     circos(R=100, cir=db, W=100,
#            mapping=outlier.link,
#            type="link",lwd=0.2,col= tmpcolor)
#   }
#
# } # end .circosmht















#   ##########################################
#   circos(R=400, cir=db, type="chr",  col=colors, print.chr.lab=TRUE, W=4, scale=TRUE);
#   circos(mapping=seg.value, W=40, cir=db)
#
#   ##########################################
#############
## EXAMPLE ##
#############
#   options(stringsAsFactors = FALSE);
#
#   set.seed(1234);
#
#   ## initial values for simulation data
#   seg.num     <- 10;
#   ind.num     <- 20;
#   seg.po      <- c(20:50);
#   link.num    <- 10;
#   link.pg.num <- 4;
#   ## output simulation data
#   sim.out <- sim.circos(seg=seg.num, po=seg.po, ind=ind.num, link=link.num,
#                         link.pg=link.pg.num);
#
#   seg.f     <- sim.out$seg.frame;
#   seg.v     <- sim.out$seg.mapping;
#   link.v    <- sim.out$seg.link
#   link.pg.v <- sim.out$seg.link.pg
#   seg.num   <- length(unique(seg.f[,1]));
#
#   ## select segments
#   seg.name <- paste("chr", 1:seg.num, sep="");
#   db       <- segAnglePo(seg.f, seg=seg.name);
#
#   colors   <- rainbow(seg.num, alpha=0.5);
#
#   pdffile  <- "OmicCircos4vignette1.pdf";
#   pdf(pdffile, 8, 8);
#   par(mar=c(2, 2, 2, 2));
#   plot(c(1,800), c(1,800), type="n", axes=FALSE, xlab="", ylab="", main="");
#
#   circos(R=400, cir=db, type="chr",  col=colors, print.chr.lab=TRUE, W=4, scale=TRUE);
#   circos(R=360, cir=db, W=40, mapping=seg.v, col.v=6, type="l",   B=TRUE, col=colors[1], lwd=2, scale=TRUE);
#   circos(R=320, cir=db, W=40, mapping=seg.v, col.v=6, type="ls",  B=FALSE, col=colors[9], lwd=2, scale=TRUE);
#   circos(R=280, cir=db, W=40, mapping=seg.v, col.v=6, type="lh",  B=TRUE, col=colors[7], lwd=2, scale=TRUE);
#   circos(R=240, cir=db, W=40, mapping=seg.v, col.v=8, type="ml",  B=FALSE, col=colors, lwd=2, scale=TRUE);
#   circos(R=200, cir=db, W=40, mapping=seg.v, col.v=8, type="ml2", B=TRUE, col=colors, lwd=2);
#   circos(R=160, cir=db, W=40, mapping=seg.v, col.v=8, type="ml3", B=FALSE, cutoff=5, lwd=2);
#   circos(R=150, cir=db, W=40, mapping=outlier.LINK[[1]], type="link", lwd=2, col=colors[c(1,ncol(link.v))]);
#   circos(R=150, cir=db, W=40, mapping=link.pg.v, type="link.pg", lwd=2, col=sample(colors,link.pg.num));
#
#   dev.off()

##############################################




# #######################
# ## .getCircleMHTPlot ##
# #######################
# .getCircleMHTPlot <- function(mainData){
#
#   colnam1 = input$Circle_y1
#   colnam2 = input$Circle_y2
#   logV1 = as.numeric(input$logV1Checkbox)
#   logV2 = as.numeric(input$logV2Checkbox)
#   poutlier = as.numeric(input$pcut)
#
#   circleplots <- NULL
#   if(!is.null(dat)){
#     if(!is.null(xSelection) && !is.null(ySelection)){
#
#       circleplots <- .circosmht(mydata=mainData,
#                                 traitsname = c(xSelection, ySelection),
#                                 pcut.outlier= cutoff,
#                                 logV1 = logx, logV2 = logy)
#     }
#   }
#   circleplots
# } # end .getCircleMHTPlot


#####################################################################################################################################




# ##############
# ## .mhtplot ##
# ##############
#
# ### Plot function, need to move to function area
#
# .mhtplot <- function(x, y,
#                      xlab, ylab,
#                      xlim=NULL, ylim=NULL,
#                      n.bins,
#                      x_sub, y_sub,
#                      col.pal, grid,
#                      outlier.col, outlier.col.bg,
#                      outlier.transp,
#                      outlier.pch, outlier.cex){
#
#   require(adegenet)
#
#   if(outlier.transp != 0){
#     outlier.transp <- 1 - outlier.transp
#     outlier.col <- transp(outlier.col, alpha = outlier.transp)
#     outlier.col.bg <- transp(outlier.col.bg, alpha = outlier.transp)
#   }
#
#   data1 <- cbind(x, y)
#   data1b <- data1[complete.cases(data1),]
#
#   # plot(1)
#
#   if(length(xlim)==0){
#     xlim_up <- max(x, na.rm=TRUE)
#     xlim_lower <- min(x, na.rm=TRUE)
#   }
#
#   if(length(ylim)==0){
#     ylim_up <- max(y, na.rm=TRUE)
#     ylim_lower <- min(y, na.rm=TRUE)
#   }
#
#   binned <- bin2(data1b,
#                  matrix(c(xlim_lower,xlim_up,ylim_lower,ylim_up), 2,2, byrow=TRUE),
#                  nbin=c(n.bins,n.bins))
#   binned$nc[binned$nc==0]=NA
#
#   x.axis.min <- xlim_lower
#   x.axis.max <- xlim_up
#   y.axis.min <- ylim_lower
#   y.axis.max <- ylim_up
#
#
#   # print(str(Chr))
#   dat <- data_outliers()
#   mynewtoy <- split(dat, "chrom")# "Chr")# # split(mydata, mydata[,Chr])
#   chrs.max <- lapply(sapply(mynewtoy,'[',"pos"),max) # lapply(sapply(mynewtoy,'[',BP),max)
#   x.total <- cumsum(as.numeric(unlist(chrs.max)))
#   x.axis.scale<-300/max(x.total)
#   x.total2<-c(0,x.total)
#
#   # plot(1)
#
#   ## CIRCULAR MANHATTAN PLOT
#   image.plot(seq(x.axis.min,x.axis.max,length.out = n.bins),
#              seq(y.axis.min, y.axis.max, length.out=n.bins),
#              binned$nc,
#              xlab=xlab, ylab=ylab, add=FALSE,
#              col=col.pal, axes=TRUE)
#
#   ## ADD OUTLIER POINTS
#   points(x_sub, y_sub, pch=outlier.pch, cex=outlier.cex, col=outlier.col, bg=outlier.col.bg)
#   # points(x=xaxis_all[data.outlier], y=yaxis_all[data.outlier], pch=18, cex=1,col="red")
#
#   #   #axis(1, at=axTicks(1), label=T)
#   #   axis(1,at=x.axis.scale*x.total2,labels=T)
#   #   axis(1,at=x.axis.scale * x.total2[-1]-diff(x.axis.scale*x.total2)/2,
#   #        labels=c(1:length(x.total)),cex=0.1,tick=F,cex.axis=0.8)
#   #   axis(2,at=axTicks(2),label=T)
#
#   ## ADD GRID
#   if(grid) grid()
#
#   ## SET TITLE TO VALUE BEING PLOTTED
#   ## NOTE: to be changed to textInput( w x- and ySelection selected)!!!!!!
#   if(!is.null(xlab) & !is.null(ylab)) title(paste(ylab, "by", xlab, sep=" "))
#
# } # end .mhtplot





















# ##########################
# ## Box: Navigation Plot ##
# ##########################
#
# # box for navigation plot
# output$box_circularManhattan_navigation <- renderUI({
#   box(title="Manhattan Plot",
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
# # subset data_outliers() ready for Manhattan plot based on selectize_circularManhattan_variables
# ManhattanData <- reactive({
#   data <- data_outliers()
#   if (is.null(input$selectize_circularManhattan_variables)) {
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
#     y <- data$y[,input$selectize_circularManhattan_variables,drop=FALSE]
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
#     par(mar=c(0.2,0.8,0.2,0.8))
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
#   par(mar=c(0.2,0.8,0.2,0.8), xpd=NA)
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
# output$box_plot_circularManhattan <- renderUI({
#   box(title=NULL, status="warning",
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
#     par(fig=c(0,1,plot_bot,plot_top),
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
#     #u <- par("usr")
#     #polygon(c(u[1],u[2],u[2],u[1]), c(u[3],u[3],u[4],u[4]), col=grey(1))
#
#     # add grid
#     tk1 <- axTicks(1)
#     tk2 <- axTicks(2)
#     abline(v=tk1, h=tk2, col=grey(0.95), lwd=1)
#
#     # finally add points and redraw frame
#     points(x, y, col=colVec, pch=20, cex=1)
#     box()
#   }
#
#   })



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
