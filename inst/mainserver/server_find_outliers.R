
########################
## FIND OUTLIERS PAGE ##  ------------------------------------------------------------------------------------
########################

## generate reactiveValues data frame of all compound distance metrics
rv_outliers_dist <- reactiveValues()
rv_outliers_dist$df_summary <- NULL
rv_outliers_dist$df_values <- NULL
rv_outliers_dist$dist <- NULL

###################################################
## Box: Controls for Producing Compound Measures ##
###################################################

# box containing controls for distance-based measure plots
output$tabBox_produce_compound <- renderUI({
  #tabBox(title=NULL, width=12, height=480,
  tabBox(title=NULL, width=12,
         tabPanel('Summary','',
                  h3('Producing Compound Measures'),
                  p('Use the tabs in this window to calculate compound distance measures. These measures take multiple columns (variables) from your original data set and calculate a single distance measure for each point in multivariate space.'),
                  p('Once you are happy with your chosen compound distance measure you can add it to the table below.'),
                  if (!is.null(rv_outliers_dist$df_summary)) {
                    dataTableOutput("distDataTable")
                  } else {
                    dataTableOutput("distDataTable_example")
                  }
         ),
         tabPanel('Distance-Based','',
                  h3('Distance-Based Methods'),
                  p('The distance between points is given by a modification of the Euclidean distance that accounts for covariance between observations.'),
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
                  hr(),
                  textInput('outliers_distance_name', label='Enter name for this measure', placeholder='(example name)'),
                  actionButton('tab2_addToTable',label='Add to table'),
                  conditionalPanel(condition='output.outliers_distance_error1 == "error"',
                                   div("Warning: this variable name is already being used. Delete existing variable to free up this variable name", style="color:red")
                                   )
         ),
         tabPanel('Density-Based','',
                  h3('Density-Based Methods'),
                  p('Kernel density based methods, with bandwidth being either user-defined, set to default, or calculated by maximum likelihood.'),
                  actionButton('button2',label='Here is a button')
         ),
         tabPanel('Use Existing','',
                  h3('Use Existing Measures'),
                  p('Use variables in the data directly as distance measures.'),
                  actionButton('button3',label='Here is a button')
         )
  )
})

# description corresponding to each distance measure
output$outliers_distance_description <- reactive({
  if (!is.null(input$outliers_distance_radio_distanceMethod)) {
    if (input$outliers_distance_radio_distanceMethod=="Mahalanobis") {
      return('The Mahalanobis distance is a multi-dimensional measure of the number of standard deviations that a point lies from the mean of a distribution. It is best suited to situations where points follow a relatively simple parametric distribution.')
    } else if (input$outliers_distance_radio_distanceMethod=="harmonic") {
      return('The harmonic mean distance (as defined here) is equal to the harmonic mean of the distance of each point from every other. Unlike the arithmetic mean, the harmonic mean is heavily influenced by small values, meaning local effects play a disproportionately large role in the final value (although global effects do still play a role).')
    } else if (input$outliers_distance_radio_distanceMethod=="nearestNeighbor") {
      return('The nearest neighbor distance is simply the smallest distance between a point and any other. Hence, it is only a measure of local effects and is not influenced by the overall distribution of the data.')
    }
  }
})

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

# table of user-defined compound distance measures
output$distDataTable <- renderDataTable({
  rv_outliers_dist$df_summary
},options=list(scrollX=TRUE, scrollY='150px', searching=FALSE, paging=FALSE) #, rownames=FALSE
)

# example table if rv_outliers_dist is empty
output$distDataTable_example <- renderDataTable({
  data.frame(name='(example name)', method='Mahalanobis', parameters='(none)', notes='foobar')
},options=list(scrollX=TRUE, scrollY='150px', searching=FALSE, paging=FALSE) #, rownames=FALSE
)

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
            rv_outliers_dist$dist <- Mahalanobis(dfv)
            #rv_outliers_dist$dist <- rnorm(nrow(dfv))
          } else if (radioChoice=='harmonic') {
            #rv_outliers_dist$dist <- harmonicDist(dfv)
            rv_outliers_dist$dist <- rgamma(nrow(dfv),2,2)
          } else if (radioChoice=='nearestNeighbor') {
            #rv_outliers_dist$dist <- neighborDist(dfv)
            rv_outliers_dist$dist <- rbeta(nrow(dfv),2,2)
          }
        }
      }
    }
  }
})

# add to table button on second tab
tab2_addToTable <- observe({
  if (!is.null(input$tab2_addToTable)) {
    if (input$tab2_addToTable>0) {
      oldNames <- isolate(rv_outliers_dist$df_summary$name)
      newName <- isolate(input$outliers_distance_name)
      if (!is.null(newName)) {
        if (!newName%in%oldNames) {
          isolate(rv_outliers_dist$df_summary <- rbind(rv_outliers_dist$df_summary, data.frame(name=newName, method='Mahalanobis', parameters='(none)', notes='foobar')))
        }
      }
    }
  }
})

######################################
## Box: Density of Compound Measure ##
######################################

#
output$box_density_compound <- renderUI({
  box(title="Density", status="warning", solidHeader=TRUE, collapsible=TRUE, width=12, height=480,
      plotOutput('plot2')
  )
})

# example plot
output$plot2 <- renderPlot({
  dist <- rv_outliers_dist$dist
  if (!is.null(dist)) {
    hist(dist, col=grey(0.8), breaks=100, main='histogram of distance measure')
  }
})


######################################
## Box: Density of Compound Measure ##
######################################

#
output$box_choose_threshold <- renderUI({
  box(title="Choose Threshold", status="primary", solidHeader=TRUE, collapsible=FALSE, width=12,
      h2('Choose Threshold'),
      p('Define a threshold (ie. a quantile) past which points are considered outliers.'),
      p('Plot this threshold on the density plot above, and use it to define the observations that make it into the table to the right.')
  )
})

######################################
## Box: Density of Compound Measure ##
######################################

#
output$box_list_outliers <- renderUI({
  box(title="Outliers", status="warning", solidHeader=TRUE, collapsible=FALSE, width=12,
      h3('(table listing outliers)'),
      h3('NOTE - perhaps this second set of boxes would be better on a seperate page?')
  )
})
