
########################
## FIND OUTLIERS PAGE ##  ------------------------------------------------------------------------------------
########################

## generate reactiveValues data frame of all compound distance metrics
rv_outliers_dist <- reactiveValues()
rv_outliers_dist$df_summary <- data.frame(name=NA, method=NA, parameters=NA, notes=NA)[-1,]
rv_outliers_dist$df_values <- NULL
rv_outliers_dist$dist <- NULL
rv_outliers_dist$deviance <- NULL

###########################
## Box: Main Outliers UI ##
###########################

# header box
output$headerBox_produce_compound <- renderUI({
  #tags$head(tags$style(HTML(".small-box {height: 50px}")))
  valueBox(
    subtitle = HTML(paste('<font size=5>Produce Compound Measures</font>')),
    color = "light-blue",
    value = NULL,
    width=12
  )
})

# box containing controls for distance-based measure plots
output$tabBox_produce_compound <- renderUI({
  tabBox(title=NULL, width=12,

         tabPanel('Summary','',
                  h3('Summary of current measures'),
                  p('Use the tabs in this window to calculate compound distance measures. These measures take multiple columns (variables) from your original data set and calculate a single distance measure for each point in multivariate space.'),
                  p('Once you are happy with your chosen compound distance measure you can add it to the table below.'),
                  dataTableOutput("distDataTable"),
                  hr(),
                  p(strong('select compound measure')),
                  fluidRow(
                    column(4,
                           selectInput('outliers_distance_chooseTableVariable', label=NULL, choices=as.character(rv_outliers_dist$df_summary$name))
                           ),
                    column(3,
                           actionButton('outliers_distance_plotFromTable',label='Plot selected', width=150)
                           ),
                    column(3,
                           actionButton('outliers_distance_deleteFromTable',label='Delete selected', width=150)
                           )
                  )
         ),

         tabPanel('Distance-Based','',
                  h3('Distance-based methods'),
                  p('These methods are all based on the distance between points in multivariate space. See help for further details of the individual methods.'),
                  hr(),
                  selectizeInput('outliers_distance_selectize_variables',
                                 label='Select univariate statistics',
                                 choices=cleanData()$otherVar,
                                 multiple=TRUE),
                  fluidRow(
                    column(4,
                           radioButtons('outliers_distance_radio_distanceMethod',
                                        label='Select distance method',
                                        choices=list('Mahalanobis distance'='Mahalanobis',
                                                     'Harmonic mean distance'='harmonic',
                                                     'Nearest neighbor distance'='nearestNeighbor')),
                           actionButton('calculate_distance',label='Calculate!')
                           ),
                    column(8,
                           wellPanel(
                             textOutput("outliers_distance_description")
                           )
                           )
                  ),
                  conditionalPanel(condition='output.outliers_distance_error2 == "error"',
                                   div("Warning: selected data contains non-numeric or missing values. Filter data in the 'Format Data' panel before calculating this distance measure.", style="color:red")
                                   ),
                  hr(),
                  fluidRow(
                    column(6,
                           textInput('outliers_distance_name', label='Enter name for this measure', placeholder='(example name)')
                           ),
                    column(6,
                           textInput('outliers_distance_note', label='Enter note for this measure', placeholder='(example note)')
                           )
                  ),
                  actionButton('tab2_addToTable',label='Add to table'),
                  conditionalPanel(condition='output.outliers_distance_error1 == "error"',
                                   div("Warning: this variable name is already being used. Delete existing variable to free up this variable name", style="color:red")
                                   )
         ),

         tabPanel('Density-Based','',
                  h3('Density-based methods'),
                  p('Kernel density based methods provide a flexible way of describing complex distributions. Points in areas of low density can then be identified as outliers.'),
                  p('One challenge with these methods is choice of the bandwidth (size) of the kernel. Three methods for choosing the bandwidth are implemented here.'),
                  hr(),
                  selectizeInput('outliers_density_selectize_variables',
                                 label='Select univariate statistics',
                                 choices=cleanData()$otherVar,
                                 multiple=TRUE),
                  fluidRow(
                    column(4,
                           radioButtons('outliers_density_howToChooseBandwidth',
                                        label='Bandwidth estimation method',
                                        choices=list('default'='default',
                                                     'manual'='manual',
                                                     'maximum likelihood'='ML')),
                           actionButton('calculate_density',label='Calculate!')
                           ),
                    column(8,
                           wellPanel(
                             conditionalPanel(condition='input.outliers_density_howToChooseBandwidth == "default"',
                                              htmlOutput("outliers_density_SilvermanDescription")
                             ),
                             conditionalPanel(condition='input.outliers_density_howToChooseBandwidth == "manual"',
                                              numericInput('outliers_density_manual_bandwidth', label='set bandwidth', value=1.0, min=0.0001, step=0.01, width=100)
                             ),
                             conditionalPanel(condition='input.outliers_density_howToChooseBandwidth == "ML"',
                                              HTML(paste('<font size=3>Choose a range of bandwidths to explore. Note that this calculation may take some time for large data sets.</font>')),
                                              fluidRow(
                                                column(3,
                                                       p(strong('min'))
                                                ),
                                                column(3,
                                                       p(strong('max'))
                                                ),
                                                column(3,
                                                       p(strong('steps'))
                                                )
                                              ),
                                              fluidRow(
                                                column(3,
                                                       numericInput('outliers_density_MLmin', label=NULL, value=0.1, min=0.0001, step=0.01)
                                                ),
                                                column(3,
                                                       numericInput('outliers_density_MLmax', label=NULL, value=0.1, min=0.0001, step=0.01)
                                                ),
                                                column(3,
                                                       numericInput('outliers_density_MLby', label=NULL, value=11, min=2, step=1)
                                                ),
                                                column(3,
                                                       actionButton('outliers_density_MLgo', label='Go!')
                                                )
                                              ),
                                              plotOutput('ML_plot')
                             )
                           )
                           )
                  ),
                  hr(),
                  fluidRow(
                    column(6,
                           textInput('outliers_density_name', label='Enter name for this measure', placeholder='(example name)')
                    ),
                    column(6,
                           textInput('outliers_density_note', label='Enter note for this measure', placeholder='(example note)')
                    )
                  ),
                  actionButton('outliers_density_addToTable',label='Add to table'),
                  conditionalPanel(condition='output.outliers_density_error1 == "error"',
                                   div("Warning: this variable name is already being used. Delete existing variable to free up this variable name", style="color:red")
                  )
         )
#          tabPanel('Use Existing','',
#                   h3('Use existing variables'),
#                   p('Use variables in the data directly as distance measures.'),
#                   actionButton('button3',label='Here is a button')
#          )
  )
})

#########################
## Tab1: Summary Table ##
#########################

# table of user-defined compound distance measures
output$distDataTable <- renderDataTable({
  rv_outliers_dist$df_summary
},options=list(scrollX=TRUE, scrollY='150px', searching=FALSE, paging=FALSE) #, rownames=FALSE
)

# plot selected button
plotFromTable <- observe({
  if (!is.null(input$outliers_distance_plotFromTable)) {
    if (input$outliers_distance_plotFromTable>0) {
      selected <- isolate(input$outliers_distance_chooseTableVariable)
      if (selected!="") {
        w <- which(rv_outliers_dist$df_summary$name==selected)
        if (length(w)>0) {
          rv_outliers_dist$dist <- rv_outliers_dist$df_values[,w]
        } else {
          rv_outliers_dist$dist <- NULL
        }
      }
    }
  }
})

# delete selected button
deleteFromTable <- observe({
  if (!is.null(input$outliers_distance_deleteFromTable)) {
    if (input$outliers_distance_deleteFromTable>0) {
      selected <- isolate(input$outliers_distance_chooseTableVariable)
      if (selected!="") {
        names <- isolate(rv_outliers_dist$df_summary$name)
        if (length(names)>0) {
          w <- which(names==selected)[1]
          isolate(rv_outliers_dist$df_summary <- rv_outliers_dist$df_summary[-w,,drop=FALSE])
          isolate(row.names(rv_outliers_dist$df_summary) <- 1:nrow(rv_outliers_dist$df_summary))
          isolate(rv_outliers_dist$df_values <- rv_outliers_dist$df_values[,-w,drop=FALSE])
          if (w==1) {
            rv_outliers_dist$dist <- NULL
          }
        }
      }
    }
  }
})

##################################
## Tab2: Distance-based Methods ##
##################################

# description corresponding to each distance measure
output$outliers_distance_description <- reactive({
  if (!is.null(input$outliers_distance_radio_distanceMethod)) {
    if (input$outliers_distance_radio_distanceMethod=="Mahalanobis") {
      return('The Mahalanobis distance is a multi-dimensional measure of the number of standard deviations that a point lies from the mean of a distribution. It is best suited to situations where points follow a relatively simple parametric distribution.')
    } else if (input$outliers_distance_radio_distanceMethod=="harmonic") {
      return('The harmonic mean distance (as defined here) is equal to the harmonic mean of the distance of each point from every other. Unlike the arithmetic mean, the harmonic mean is heavily influenced by small values meaning local effects play a large role in the final value, although global effects do still play a role.')
    } else if (input$outliers_distance_radio_distanceMethod=="nearestNeighbor") {
      return('The nearest neighbor distance is simply the smallest distance between a point and any other. Hence, it is only a measure of local effects and is not influenced by the overall distribution of the data.')
    }
  }
})

# button to calculate distances
calculate_distance <- observe({
  if (!is.null(input$calculate_distance)) {
    if (input$calculate_distance>0) {
      selectedVars <- isolate(input$outliers_distance_selectize_variables)
      if (!is.null(selectedVars)) {
        dfv <- isolate(cleanData()$y[,selectedVars,drop=FALSE])
        radioChoice <- isolate(input$outliers_distance_radio_distanceMethod)
        if (!is.null(radioChoice)) {
          if (radioChoice=='Mahalanobis') {
            if (!any(mapply(function(x){any(is.na(x))}, dfv)) & all(mapply(is.numeric,dfv))) {
              rv_outliers_dist$dist <- Mahalanobis(dfv)
            }
          } else if (radioChoice=='harmonic') {
            if (!any(mapply(function(x){any(is.na(x))}, dfv)) & all(mapply(is.numeric,dfv))) {
              rv_outliers_dist$dist <- harmonicDist(dfv)
            }
          } else if (radioChoice=='nearestNeighbor') {
            if (!any(mapply(function(x){any(is.na(x))}, dfv)) & all(mapply(is.numeric,dfv))) {
              rv_outliers_dist$dist <- neighborDist(dfv)
            }
          }
        }
      }
    }
  }
})

# error if any problematic NAs in data (activates conditional panel)
output$outliers_distance_error2 <- reactive({
  selectedVars <- input$outliers_distance_selectize_variables
  if (!is.null(selectedVars)) {
    dfv <- isolate(cleanData()$y[,selectedVars,drop=FALSE])
    if (any(mapply(function(x){any(is.na(x))}, dfv)) | any(!mapply(is.numeric,dfv))) {
      return('error')
    } else {
      return('no_error')
    }
  }
})
outputOptions(output, 'outliers_distance_error2', suspendWhenHidden=FALSE)

# error if name already being used (activates conditional panel)
output$outliers_distance_error1 <- reactive({
  if (!is.null(input$outliers_distance_name)) {
    if (input$outliers_distance_name%in%rv_outliers_dist$df_summary$name) {
      return('error')
    } else {
      return('no_error')
    }
  }
})
outputOptions(output, 'outliers_distance_error1', suspendWhenHidden=FALSE)

# add to table button
tab2_addToTable <- observe({
  if (!is.null(input$tab2_addToTable)) {
    if (input$tab2_addToTable>0) {
      oldNames <- isolate(rv_outliers_dist$df_summary$name)
      newName <- isolate(input$outliers_distance_name)
      note <- isolate(input$outliers_distance_note)
      distMethod <- isolate(input$outliers_distance_radio_distanceMethod)
      if (!is.null(newName) & !is.null(distMethod)) {
        if (!newName%in%oldNames & newName!="") {
          if (distMethod=="Mahalanobis") {
            isolate(rv_outliers_dist$df_summary <- rbind(rv_outliers_dist$df_summary, data.frame(name=newName, method='Mahalanobis', parameters='(none)', notes=note)))
          } else if (distMethod=="harmonic") {
            isolate(rv_outliers_dist$df_summary <- rbind(rv_outliers_dist$df_summary, data.frame(name=newName, method='harmonic', parameters='(none)', notes=note)))
          }
          else if (distMethod=="nearestNeighbor") {
            isolate(rv_outliers_dist$df_summary <- rbind(rv_outliers_dist$df_summary, data.frame(name=newName, method='nearest neighbor', parameters='(none)', notes=note)))
          }
          isolate(row.names(rv_outliers_dist$df_summary) <- 1:nrow(rv_outliers_dist$df_summary))
          isolate(rv_outliers_dist$df_values <- cbind(rv_outliers_dist$df_values, rv_outliers_dist$dist))
        }
      }
    }
  }
})

#################################
## Tab3: Density-based Methods ##
#################################

# description of Silverman's rule
output$outliers_density_SilvermanDescription <- reactive({
  if (!is.null(input$outliers_density_howToChooseBandwidth)) {
    if (input$outliers_density_howToChooseBandwidth=="default") {
      return(HTML(paste("Bandwidth will be chosen via Silverman's rule, which in this case yields a value of <b>",0.5,"</b>.",sep="")))
    }
  }
})

# button to get ML bandwidth
outliers_density_MLgo <- observe({
  if (!is.null(input$outliers_density_MLgo)) {
    if (input$outliers_density_MLgo>0) {
      MLmin <- isolate(input$outliers_density_MLmin)
      MLmax <- isolate(input$outliers_density_MLmax)
      MLby <- isolate(input$outliers_density_MLby)
      if (MLmin>0 & MLmax>MLmin) {
        MLvec <- seq(MLmin, MLmax, l=MLby)
        selectedVars <- isolate(input$outliers_density_selectize_variables)
        if (!is.null(selectedVars)) {
          dfv <- isolate(cleanData()$y[,selectedVars,drop=FALSE])
          if (!any(mapply(function(x){any(is.na(x))}, dfv)) & all(mapply(is.numeric,dfv))) {
            print('calculating...')
            rv_outliers_dist$deviance <- NULL
            for (i in 1:length(MLvec)) {
              print(i)
              rv_outliers_dist$deviance <- c(rv_outliers_dist$deviance, kernelDeviance(dfv, bandwidth=MLvec[i]))
            }
          }
        }
      }
    }
  }
})

# bandwidth ML plot
output$ML_plot <- renderPlot({
  deviance <- rv_outliers_dist$deviance
  if (!is.null(deviance)) {
    plot(deviance, type='o', pch=20, lwd=1.5)
  }
})


########################################
## Box: Histogram of Compound Measure ##
########################################

# box for histogram
output$box_histogram_compound <- renderUI({
  box(title="Histogram", status="warning", solidHeader=TRUE, collapsible=TRUE, width=12,
      plotOutput('histogram_compound')
  )
})

# hist plot
output$histogram_compound <- renderPlot({
  dist <- rv_outliers_dist$dist
  if (!is.null(dist)) {
    hist(dist, col=grey(0.8), breaks=100, xlab='compound distance measure', main='')
  }
})


######################################
## Box: Density of Compound Measure ##
######################################

#
# output$box_choose_threshold <- renderUI({
#   box(title="Choose Threshold", status="primary", solidHeader=TRUE, collapsible=FALSE, width=12,
#       h2('Choose Threshold'),
#       p('Define a threshold (ie. a quantile) past which points are considered outliers.'),
#       p('Plot this threshold on the density plot above, and use it to define the observations that make it into the table to the right.')
#   )
# })
#
# ######################################
# ## Box: Density of Compound Measure ##
# ######################################
#
# #
# output$box_list_outliers <- renderUI({
#   box(title="Outliers", status="warning", solidHeader=TRUE, collapsible=FALSE, width=12,
#       h3('(table listing outliers)'),
#       h3('NOTE - perhaps this second set of boxes would be better on a seperate page?')
#   )
# })
