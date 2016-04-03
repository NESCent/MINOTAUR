
#################################
## PLOT HISTOGRAM/DENSITY PAGE ##  ------------------------------------------------------------------------------------
#################################

#######################################################
## Box: Global Controls for Univariate Distributions ##
#######################################################

## Fn to generate boxes containing controls for univariate distribution plots

.get.hist_1D.controls <- function(dat){

  out <- NULL
  var.choices <- var.sel <- NULL

  if(!is.null(dat)){

    ## get numeric variables
    numCols <- which(sapply(c(1:ncol(dat$y)),
                            function(e) is.numeric(dat$y[,e])))

    var.choices <- c("Position", "Chromosome", names(dat$y)[numCols])

    var.sel <- var.choices[3]
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

        box(title="Select a variable to plot:", # "Univariate Distributions"
            status="info",
            #status = "primary",
            solidHeader=TRUE,
            collapsible=TRUE,
            width=12,

            ## NOTE: Would like to be able to pull the Chromosome and Position variables
            ## selected/generated in the Format Data tab to be available as options
            ## and autoatically selected below...

            ## Variable to plot
            selectizeInput("hist_1D_var",
                           label='Variable:',
                           choices= var.choices,
                           selected = var.sel,
                           multiple=FALSE)
        )

        )

  return(out)
} # end .get.hist_1D.controls



## Generate K individual controls for each univariate plot,
## produced using lapply method, K taken from actionButton:
output$box_hist_1D_controls <- renderUI({
  k <- 1
  k <- input$new_hist_1D_button[1] + 1
  if(length(k) > 0){
    if(k > 0) {
      lapply(1:k,function(i){
        .get.hist_1D.controls(cleanData())
      })
    }
  }
})


###################################
## .get.hist_1D.controls.aes ##
###################################
## fn to get widgets to control plot AESTHETICS under plot
.get.hist_1D.controls.aes <- function(dat){

  out <- NULL

  out <-
    box(title="Adjust Plot Aesthetics:",
        status="warning",
        solidHeader=TRUE,
        collapsible=TRUE,
        width=12,

        sliderInput("hist_1D_n.bins",
                    label = "Number of bins:",
                    min = 2, max = 1000,
                    value = 100,
                    step = 1)

        #         ## Number of bins
        #         textInput("hist_1D_nbins",
        #                   label = "Number of bins",
        #                   value = 100)

    ) # end box

  return(out)

} # end .get.scatterPlot.controls.aes


## Generate K individual controls for each univariate plot,
## produced using lapply method, K taken from actionButton:
output$box_hist_1D_controls_aes <- renderUI({
  k <- 1
  k <- input$new_hist_1D_button[1] + 1
  if(length(k) > 0){
    if(k > 0) {
      lapply(1:k,function(i){
        .get.hist_1D.controls.aes(cleanData())
      })
    }
  }
})





####################################
## BUTTON: Generate another plot? ##
####################################
output$box_hist_1D_button <- renderUI({
  box(
    title = "Generate another plot?",
    solidHeader = TRUE,
    status = "primary",
    value = NULL,
    width=12,

    ## button
    actionButton(inputId = "new_hist_1D_button",
                 label = "Yes, please!",
                 icon = icon("cog"))
  )
})

# output$buttonTest_hist_1D <- renderPrint({
#   print(input$new_hist_1D_button[1])
# })




#######################
## Box: hist_1D Plot ##
#######################

output$box_hist_1D_plot <- renderUI({
  box(title="Histogram",
      status="warning",
      solidHeader=TRUE,
      collapsible=TRUE,
      width=12,

      # h3("Histogram"),

      plotOutput("plot_hist_1D_plot")
  )
})


# output$foo <- renderText({
#   dat <- var <- toPlot <- NULL
#   dat <- cleanData()
#   var <- input$hist_1D_var
# })


########################
## Plot: hist_1D_plot ##
########################

## set n.bins reactiveValue:
rv <- reactiveValues()
rv$n.bins <- 100 # set to initial selected value of textInput n.bins


## get hist plot output:
output$plot_hist_1D_plot <- renderPlot({

  dat <- var <- toPlot <- NULL

  dat <- cleanData()
  var <- input$hist_1D_var

  if(!is.null(var)){
    ## Get variable to plot
    if(var == "Position"){
      ## could be used to check for missing values...
      # toPlot <- eval(parse(text="cleanData()$pos"))
      toPlot <- eval(parse(text="dat$pos"))
    }else{
      if(var == "Chromosome"){
        ## could be used to check for representation/length of each chromosome
        # toPlot <- eval(parse(text="cleanData()$chrom"))
        toPlot <- eval(parse(text="dat$chrom"))
      }else{
        # toPlot <- eval(parse(text=paste("cleanData()$y", var, sep="$")))
        toPlot <- eval(parse(text=paste("dat$y", var, sep="$")))
      }
    }

    ## Get n.bins
    n.bins <- as.numeric(input$hist_1D_n.bins)
    #     if(!is.null(input$hist_1D_nbins)){
    #       if(length(input$hist_1D_nbins) > 0){
    #         if(input$hist_1D_nbins != 0 &
    #            input$hist_1D_nbins != "" &
    #            input$hist_1D_nbins != " " &
    #            input$hist_1D_nbins != "\""){
    #           ## check that input is a number
    #           if(all.is.numeric(input$hist_1D_nbins)){
    #             ## and a whole number
    #             if(suppressWarnings(.is.wholenumber(as.numeric(input$hist_1D_nbins)))){
    #               ## check that n.bins is a value
    #               ## (ie. not an empty reactivevalues object)
    #               if(!is.null(rv$n.bins)){
    #                 ## check that n.bins is not already set to input value
    #                 if(input$hist_1D_nbins != rv$n.bins){
    #                   rv$n.bins <- input$hist_1D_nbins
    #                   rv$n.bins <- as.numeric(rv$n.bins)
    #                 }
    #               }
    #             }
    #           }
    #         }
    #       }
    #     }

    ## PLOT HISTOGRAM
    if(!is.null(toPlot)){
      if(!is.null(rv$n.bins)){
        hist(toPlot, breaks=n.bins, main=NULL)
      }
    }
    ## SET TITLE TO VALUE BEING HISTOGRAMIFIED
    title(var) # to be changed to textInput( w var selected)
  }

})

# ###########################
# ## Box: Navigation panel ##
# ###########################
# output$box_hist_1D_navigation <- renderUI({
#   box(title="Adjust plot aesthetics:",
#       status="warning",
#       solidHeader=FALSE,
#       collapsible=TRUE,
#       width=12
#   )
# })
