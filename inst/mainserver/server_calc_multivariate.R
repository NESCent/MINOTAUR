
########################
## FIND OUTLIERS PAGE ##  ------------------------------------------------------------------------------------
########################

################################
## Functions for Loading Bars ##
################################

# version of harmonicDist for GUI that calculates in chunks, allowing progress bar to be updated in between steps
.harmonicDist_partial <- function(dfv, S_inv, i_start, i_end){
  d <- ncol(dfv)
  distances <- C_harmonicDist_partial(split(t(dfv),1:d), split(S_inv,1:d), i_start, i_end)$distance
  return(distances)
}

# version of neighborDist for GUI that calculates in chunks, allowing progress bar to be updated in between steps
.neighborDist_partial <- function(dfv, S_inv, i_start, i_end){
    d <- ncol(dfv)
    distances <- C_neighborDist_partial(split(t(dfv),1:d), split(S_inv,1:d), i_start, i_end)$distance
    return(distances)
}

# version of kernelDist for GUI that calculates in chunks, allowing progress bar to be updated in between steps
.kernelDist_partial <- function(dfv, bandwidth, S_inv, i_start, i_end){
    d <- ncol(dfv)
    distances <- C_kernelDist_partial(split(t(dfv),1:d), bandwidth^2, split(S_inv,1:d), i_start, i_end)$distance
    return(distances)
}

# version of kernelDeviance for GUI that calculates in chunks, allowing progress bar to be updated in between steps
.kernelDeviance_partial <- function(dfv, bandwidth, S_inv, i_start, i_end){
    d <- ncol(dfv)
    deviance <- C_kernelDeviance_partial(split(t(dfv),1:d), bandwidth^2, split(S_inv,1:d), i_start, i_end)$deviance
    return(deviance)
}

############################
## Define Reactive Values ##
############################

## generate reactiveValues object for all compound distance metric related quantities
rv_outliers <- reactiveValues()
rv_outliers$df_summary <- data.frame(name=NA, method=NA, parameters=NA, notes=NA)[-1,]
rv_outliers$df_values <- NULL
rv_outliers$dist <- NULL
rv_outliers$deviance <- NULL
rv_outliers$bandwidth <- NULL

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
                           selectInput('outliers_distance_chooseTableVariable', label=NULL, choices=as.character(rv_outliers$df_summary$name))
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
                  p('These methods are all based on the distance between points in multivariate space. See the MINOTAUR manuscript for further details of the individual methods.'),
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
                  p('Kernel density based methods provide a flexible way of describing complex distributions. Points in areas of low density are potential outliers.'),
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
                                                       numericInput('outliers_density_MLmin', label=NULL, value=0.2, min=0.0001, step=0.01)
                                                ),
                                                column(3,
                                                       numericInput('outliers_density_MLmax', label=NULL, value=1.0, min=0.0001, step=0.01)
                                                ),
                                                column(3,
                                                       numericInput('outliers_density_MLby', label=NULL, value=5, min=2, step=1)
                                                ),
                                                column(3,
                                                       actionButton('outliers_density_MLgo', label='Go!')
                                                )
                                              ),
                                              plotOutput('ML_plot', height=350),
                                              htmlOutput("outliers_density_MLdescription")
                             )
                           )
                    )
                  ),
                  conditionalPanel(condition='output.outliers_density_error2 == "error"',
                                   div("Warning: selected data contains non-numeric or missing values. Filter data in the 'Format Data' panel before calculating this distance measure.", style="color:red")
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
  rv_outliers$df_summary
},options=list(scrollX=TRUE, scrollY='150px', searching=FALSE, paging=FALSE) #, rownames=FALSE
)

# plot selected button
plotFromTable <- observe({
  if (!is.null(input$outliers_distance_plotFromTable)) {
    if (input$outliers_distance_plotFromTable>0) {
      selected <- isolate(input$outliers_distance_chooseTableVariable)
      if (selected!="") {
        w <- which(rv_outliers$df_summary$name==selected)
        if (length(w)>0) {
          rv_outliers$dist <- rv_outliers$df_values[,w]
        } else {
          rv_outliers$dist <- NULL
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
        names <- isolate(rv_outliers$df_summary$name)
        if (length(names)>0) {
          w <- which(names==selected)[1]
          isolate(rv_outliers$df_summary <- rv_outliers$df_summary[-w,,drop=FALSE])
          isolate(rv_outliers$df_values <- rv_outliers$df_values[,-w,drop=FALSE])
          if ((length(names)-1)>0) {
            isolate(row.names(rv_outliers$df_summary) <- 1:nrow(rv_outliers$df_summary))
          } else {
            rv_outliers$dist <- NULL
          }
        }
      }
    }
  }
})

##################################
## Tab2: Distance-based Methods ##
##################################

# hide histogram when changing method
outliers_distance_radio_distanceMethod <- observe({
  if (!is.null(input$outliers_distance_radio_distanceMethod)) {
    rv_outliers$dist <- NULL
  }
})

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

              # calculate Mahalanobis distance
              withProgress(message="Calculating Mahalanobis", value=0, {
                rv_outliers$dist <- Mahalanobis(dfv)
                incProgress(1)
              })


            }
          } else if (radioChoice=='harmonic') {
            if (!any(mapply(function(x){any(is.na(x))}, dfv)) & all(mapply(is.numeric,dfv))) {

              # calculate harmonic mean distance
              withProgress(message="Calculating harmonic mean", value=0, {
                S <- cov(dfv)
                S_inv <- solve(S)
                breakVec <- c(floor(nrow(dfv)/100)*(0:99), nrow(dfv))
                distances <- rep(NA,nrow(dfv))
                for (i in 2:length(breakVec)) {
                  i_start <- breakVec[i-1]+1
                  i_end <- breakVec[i]
                  distances[i_start:i_end] <- .harmonicDist_partial(dfv, S_inv, i_start, i_end)
                  incProgress(1/100, detail=paste(i-1,"%",sep=""))
                }
                rv_outliers$dist <- distances
              })

            }
          } else if (radioChoice=='nearestNeighbor') {
            if (!any(mapply(function(x){any(is.na(x))}, dfv)) & all(mapply(is.numeric,dfv))) {

              # calculate nearest neighbour distance
              withProgress(message="Calculating nearest neighbor", value=0, {
                S <- cov(dfv)
                S_inv <- solve(S)
                breakVec <- c(floor(nrow(dfv)/100)*(0:99), nrow(dfv))
                distances <- rep(NA,nrow(dfv))
                for (i in 2:length(breakVec)) {
                  i_start <- breakVec[i-1]+1
                  i_end <- breakVec[i]
                  distances[i_start:i_end] <- .neighborDist_partial(dfv, S_inv, i_start, i_end)
                  incProgress(1/100, detail=paste(i-1,"%",sep=""))
                }
                rv_outliers$dist <- distances
              })

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

# # version for GUI that calculates in chunks, allowing progress bar to be updated in between steps
# .harmonicDist_partial <- function(dfv, S_inv, i_start, i_end){
#   d <- ncol(dfv)
#   distances <- C_harmonicDist_partial(split(t(dfv),1:d), split(S_inv,1:d), i_start, i_end)$distance
#   return(distances)
# }


# error if name already being used (activates conditional panel)
output$outliers_distance_error1 <- reactive({
  if (!is.null(input$outliers_distance_name)) {
    if (input$outliers_distance_name%in%rv_outliers$df_summary$name) {
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
      if (!is.null(isolate(rv_outliers$dist))) {
        oldNames <- isolate(rv_outliers$df_summary$name)
        newName <- isolate(input$outliers_distance_name)
        note <- isolate(input$outliers_distance_note)
        distMethod <- isolate(input$outliers_distance_radio_distanceMethod)
        if (!is.null(newName) & !is.null(distMethod)) {
          if (!newName%in%oldNames & newName!="") {
            if (distMethod=="Mahalanobis") {
              isolate(rv_outliers$df_summary <- rbind(rv_outliers$df_summary, data.frame(name=newName, method='Mahalanobis', parameters='(none)', notes=note)))
            } else if (distMethod=="harmonic") {
              isolate(rv_outliers$df_summary <- rbind(rv_outliers$df_summary, data.frame(name=newName, method='harmonic', parameters='(none)', notes=note)))
            }
            else if (distMethod=="nearestNeighbor") {
              isolate(rv_outliers$df_summary <- rbind(rv_outliers$df_summary, data.frame(name=newName, method='nearest neighbor', parameters='(none)', notes=note)))
            }
            isolate(row.names(rv_outliers$df_summary) <- 1:nrow(rv_outliers$df_summary))
            isolate(rv_outliers$df_values <- cbind(rv_outliers$df_values, rv_outliers$dist))
          }
        }
      }
    }
  }
})

#################################
## Tab3: Density-based Methods ##
#################################

# change bandwidth based on method selection
outliers_density_howToChooseBandwidth <- observe({
  if (!is.null(input$outliers_density_howToChooseBandwidth)) {
    rv_outliers$dist <- NULL
    rv_outliers$bandwidth <- NULL
    if (input$outliers_density_howToChooseBandwidth=="default") {

      # defualt bandwidth
      selectedVars <- input$outliers_density_selectize_variables
      if (!is.null(selectedVars)) {
        dfv <- isolate(cleanData()$y[,selectedVars,drop=FALSE])
        n <- nrow(dfv)
        d <- ncol(dfv)
        bandwidth <- (4/(d+2))^(1/(d+4))*n^(-1/(d+4))
        rv_outliers$bandwidth <- max(round(bandwidth,3), 0.001)
      }

    } else if (input$outliers_density_howToChooseBandwidth=="manual") {

      # manual bandwidth
      bandwidth <- isolate(input$outliers_density_manual_bandwidth)
      if (!is.null(bandwidth)) {
        rv_outliers$bandwidth <- max(round(bandwidth,3), 0.001)
      }

    } else if (input$outliers_density_howToChooseBandwidth=="ML") {

      # ML bandwidth
      deviance <- isolate(rv_outliers$deviance)
      MLvec <- isolate(rv_outliers$MLvec)
      if (!is.null(deviance)) {
        bandwidth <- MLvec[which.min(deviance)]
        rv_outliers$bandwidth <- max(round(bandwidth,3), 0.001)
      }

    }
  }
})

# description of Silverman's rule
output$outliers_density_SilvermanDescription <- reactive({
  if (!is.null(input$outliers_density_howToChooseBandwidth)) {
    if (input$outliers_density_howToChooseBandwidth=="default") {
      if (is.null(rv_outliers$bandwidth)) {
        return(HTML("Bandwidth will be chosen via Silverman's rule."))
      } else {
        return(HTML(paste("Bandwidth will be chosen via Silverman's rule, which in this case yields a value of <b>",rv_outliers$bandwidth,"</b>.",sep="")))
      }
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

            # get ML bandwidth
            S <- cov(dfv)
            S_inv <- solve(S)
            breakVec <- c(floor(nrow(dfv)/20)*(0:19), nrow(dfv))
            deviance <- rep(NA,length(MLvec))
            withProgress(message="Exploring bandwidth:", value=0, {
              for (j in 1:length(MLvec)) {
                deviance[j] <- 0
                incProgress(1/length(MLvec), detail=paste(signif(MLvec[j],3), " (", round(j/length(MLvec)*100), "%)", sep=""))
                withProgress(message="Calculating deviance", value=0, {
                  for (i in 2:length(breakVec)) {
                    i_start <- breakVec[i-1]+1
                    i_end <- breakVec[i]
                    deviance[j] <- deviance[j] + .kernelDeviance_partial(dfv, MLvec[j], S_inv, i_start, i_end)
                    incProgress(1/20, detail=paste((i-1)/20*100,"%",sep=""))
                  }
                })
                rv_outliers$deviance <- deviance
                rv_outliers$MLvec <- MLvec
                rv_outliers$bandwidth <- max(round(MLvec[which.min(deviance)],3), 0.001)
              }
            })

          }
        }
      }
    }
  }
})

# bandwidth ML plot
output$ML_plot <- renderPlot({
  deviance <- rv_outliers$deviance
  MLvec <- rv_outliers$MLvec
  if (!is.null(deviance) & !is.null(MLvec)) {
    plot(MLvec, deviance, type='o', pch=20, lwd=1.5, xlab='bandwidth', ylab='deviance')
    x_best <- MLvec[which.min(deviance)]
    y_min <- min(deviance)
    y_max <- max(deviance)
    y_top <- y_min + 0.25*(y_max-y_min)
    arrows(x_best,y_top,x_best,y_min,lwd=2,col='#64b4ff')
  }
})

# text output final ML value
output$outliers_density_MLdescription <- reactive({
  if (!is.null(rv_outliers$MLvec)) {
    ML_min <- rv_outliers$MLvec[which.min(rv_outliers$deviance)]
    bandwidth <- round(ML_min,digits=3)
    if (bandwidth<0.0001)
      bandwidth <- 0.0001
    return(HTML(paste("<font size=3>Maximum likelihood bandwidth: <b>",bandwidth,"</b>.</font>",sep="")))
  }
})

# button to calculate kernel density deviance
calculate_density <- observe({
  if (!is.null(input$calculate_density)) {
    if (input$calculate_density>0) {
      selectedVars <- isolate(input$outliers_density_selectize_variables)
      if (!is.null(selectedVars)) {
        dfv <- isolate(cleanData()$y[,selectedVars,drop=FALSE])
        radioChoice <- isolate(input$outliers_density_howToChooseBandwidth)
        if (!is.null(radioChoice)) {

          if (radioChoice=='default') {
            if (!any(mapply(function(x){any(is.na(x))}, dfv)) & all(mapply(is.numeric,dfv))) {

              # calculate kernel density distance using default bandwidth
              withProgress(message="Calculating kernel distance", value=0, {
                S <- cov(dfv)
                S_inv <- solve(S)
                breakVec <- c(floor(nrow(dfv)/100)*(0:99), nrow(dfv))
                distances <- rep(NA,nrow(dfv))
                for (i in 2:length(breakVec)) {
                  i_start <- breakVec[i-1]+1
                  i_end <- breakVec[i]
                  distances[i_start:i_end] <- .kernelDist_partial(dfv, isolate(rv_outliers$bandwidth), S_inv, i_start, i_end)
                  incProgress(1/100, detail=paste(i-1,"%",sep=""))
                }
                rv_outliers$dist <- distances
              })

            }
          } else if (radioChoice=='manual') {
            if (!any(mapply(function(x){any(is.na(x))}, dfv)) & all(mapply(is.numeric,dfv))) {

              # calculate kernel density distance using manual bandwidth
              withProgress(message="Calculating kernel distance", value=0, {
                S <- cov(dfv)
                S_inv <- solve(S)
                breakVec <- c(floor(nrow(dfv)/100)*(0:99), nrow(dfv))
                distances <- rep(NA,nrow(dfv))
                for (i in 2:length(breakVec)) {
                  i_start <- breakVec[i-1]+1
                  i_end <- breakVec[i]
                  distances[i_start:i_end] <- .kernelDist_partial(dfv, isolate(rv_outliers$bandwidth), S_inv, i_start, i_end)
                  incProgress(1/100, detail=paste(i-1,"%",sep=""))
                }
                rv_outliers$dist <- distances
              })

            }
          } else if (radioChoice=='ML') {
            if (!any(mapply(function(x){any(is.na(x))}, dfv)) & all(mapply(is.numeric,dfv))) {

              # get ML bandwidth
              deviance <- isolate(rv_outliers$deviance)
              MLvec <- isolate(rv_outliers$MLvec)
              if (!is.null(deviance)) {
                bandwidth <- isolate(MLvec[which.min(deviance)])

                # calculate kernel density distance using ML bandwidth
                bandwidth <- isolate(input$outliers_density_manual_bandwidth)
                withProgress(message="Calculating kernel distance", value=0, {
                  S <- cov(dfv)
                  S_inv <- solve(S)
                  breakVec <- c(floor(nrow(dfv)/100)*(0:99), nrow(dfv))
                  distances <- rep(NA,nrow(dfv))
                  for (i in 2:length(breakVec)) {
                    i_start <- breakVec[i-1]+1
                    i_end <- breakVec[i]
                    distances[i_start:i_end] <- .kernelDist_partial(dfv, bandwidth, S_inv, i_start, i_end)
                    incProgress(1/100, detail=paste(i-1,"%",sep=""))
                  }
                  rv_outliers$dist <- distances
                })

              }

            }
          }
        }
      }
    }
  }
})

# error if any problematic NAs in data (activates conditional panel)
output$outliers_density_error2 <- reactive({
  selectedVars <- input$outliers_density_selectize_variables
  if (!is.null(selectedVars)) {
    dfv <- isolate(cleanData()$y[,selectedVars,drop=FALSE])
    if (any(mapply(function(x){any(is.na(x))}, dfv)) | any(!mapply(is.numeric,dfv))) {
      return('error')
    } else {
      return('no_error')
    }
  }
})
outputOptions(output, 'outliers_density_error2', suspendWhenHidden=FALSE)

# error if name already being used (activates conditional panel)
output$outliers_density_error1 <- reactive({
  if (!is.null(input$outliers_density_name)) {
    if (input$outliers_density_name%in%rv_outliers$df_summary$name) {
      return('error')
    } else {
      return('no_error')
    }
  }
})
outputOptions(output, 'outliers_density_error1', suspendWhenHidden=FALSE)

# add to table button
outliers_density_addToTable <- observe({
  if (!is.null(input$outliers_density_addToTable)) {
    if (input$outliers_density_addToTable>0) {
      if (!is.null(isolate(rv_outliers$dist))) {
        oldNames <- isolate(rv_outliers$df_summary$name)
        newName <- isolate(input$outliers_density_name)
        note <- isolate(input$outliers_density_note)
        if (!is.null(newName)) {
          if (!newName%in%oldNames & newName!="") {
            isolate(rv_outliers$df_summary <- rbind(rv_outliers$df_summary, data.frame(name=newName, method='Kernel density deviance', parameters=paste('bandwidth=', isolate(rv_outliers$bandwidth), sep=""), notes=note)))
            isolate(row.names(rv_outliers$df_summary) <- 1:nrow(rv_outliers$df_summary))
            isolate(rv_outliers$df_values <- cbind(rv_outliers$df_values, rv_outliers$dist))
          }
        }
      }
    }
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
  dist <- rv_outliers$dist
  if (!is.null(dist)) {
    hist(dist, col=grey(0.8), breaks=100, xlab='compound distance measure', main='')
  }
})


################################################
## Output Data and Compound Distance Measures ##
################################################

data_outliers <- reactive({

  data <- cleanData()
  df_values <- rv_outliers$df_values
  if (!is.null(df_values)) {
    df_values <- as.data.frame(df_values)
    names(df_values) <- rv_outliers$df_summary$name
    data$y <- cbind(data$y, df_values)
  }

  return(data)
})


