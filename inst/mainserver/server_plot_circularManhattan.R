

##################################
## CIRCULAR MANHATTAN PLOT PAGE ##  ------------------------------------------------------------------------------------
##################################

#######################################################
## Box: Global Controls for Univariate Distributions ##
#######################################################

## Fn to generate boxes containing controls for univariate distribution plots

.get.circularManhattan.controls <- function(dat){

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
            selectizeInput('circularManhattan_xaxis',
                           label = 'X-axis:',
                           choices = x.var.choices,
                           selected = x.var.sel,
                           multiple = FALSE),

            ## log(x-axis) ?
            radioButtons("circularManhattan_logx",
                         label = "Log x-axis?",
                         choices = list("log2", "log10", "none"),
                         selected="none",
                         inline=TRUE),

            ## Flip x-axis ?
            radioButtons("circularManhattan_flipx",
                         label = "Invert x-axis?",
                         choices = list("Yes", "No"),
                         selected="none",
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
            selectizeInput('circularManhattan_yaxis',
                           label = 'Y-axis:',
                           choices = y.var.choices,
                           selected = y.var.sel,
                           multiple = FALSE),

            ## log(y-axis) ?
            radioButtons("circularManhattan_logy",
                         label = "Log y-axis?",
                         choices = list("log2", "log10", "none"),
                         selected="none",
                         inline=TRUE),

            ## Flip y-axis ?
            radioButtons("circularManhattan_flipy",
                         label = "Invert y-axis?",
                         choices = list("Yes", "No"),
                         selected="none",
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
            selectizeInput('circularManhattan_var2',
                           label = NULL,
                           choices = o.var.choices,
                           selected = o.var.sel,
                           multiple = FALSE),

            ## Cut-off for outliers to overlay
            # eg 0.01
            textInput("circularManhattan_cutoff",
                      label = "Cut-off for outliers to overlay",
                      value = 0.05)
            )

        )

  return(out)
} # end .get.circularManhattan.controls



## Generate K individual controls for each univariate plot,
## produced using lapply method, K taken from actionButton:
output$box_circularManhattan_controls <- renderUI({
  k <- 1
  k <- input$new_circularManhattan_button[1] + 1
  if(length(k) > 0){
    if(k > 0) {
      lapply(1:k,function(i){
        .get.circularManhattan.controls(cleanData())
      })
    }
  }
})


###################################
## .get.circularManhattan.controls.aes ##
###################################
## fn to get widgets to control plot AESTHETICS under plot
.get.circularManhattan.controls.aes <- function(dat){

  out <- NULL

  out <-
    box(title="Adjust Plot Aesthetics:",
        status="warning",
        solidHeader=TRUE,
        collapsible=TRUE,
        width=12,

        ## Number of bins
        textInput("circularManhattan_nbins",
                  label = "Number of bins",
                  value = 100)

    ) # end box

  return(out)

} # end .get.circularManhattan.controls.aes


## Generate K individual controls for each univariate plot,
## produced using lapply method, K taken from actionButton:
output$box_circularManhattan_controls_aes <- renderUI({
  k <- 1
  k <- input$new_circularManhattan_button[1] + 1
  if(length(k) > 0){
    if(k > 0) {
      lapply(1:k,function(i){
        .get.circularManhattan.controls.aes(cleanData())
      })
    }
  }
})


####################################
## BUTTON: Generate another plot? ##
####################################
output$box_circularManhattan_button <- renderUI({
  box(
    title = "Generate another plot?",
    solidHeader = TRUE,
    status = "primary",
    #background = "light-blue",
    value = NULL,
    width=12,

    ## background color:
    #     tags$head(
    #       tags$style(HTML('#new_circularManhattan_button{background-color:00ccff}'))
    #     ),

    ## button
    actionButton(inputId = "new_circularManhattan_button",
                 label = "Yes, please!",
                 icon = icon("cog"))
  )
})

# output$buttonTest_circularManhattan <- renderPrint({
#   print(input$new_circularManhattan_button[1])
# })


