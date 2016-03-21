
########################
## FIND OUTLIERS PAGE ##  ------------------------------------------------------------------------------------
########################

###################################################
## Box: Controls for Producing Compound Measures ##
###################################################

# box containing controls for distance-based measure plots
output$tabBox_produce_compound <- renderUI({
  tabBox(title=NULL, width=12, height=480,
         tabPanel('Summary','',
                  h3('Producing Compound Measures'),
                  p('This tab will contain an introduction to the idea of using multiple univariate measures to compute compound measures.'),
                  p('It will also contain a table showing a list of currently used compound measures. The user will be able to create new measures to add to this list, and delete from this list.')
         ),
         tabPanel('Distance-Based','',
                  h3('Distance-Based Methods'),
                  p('Mahalanobis, harmonic mean and nearest neighbour distances.'),
                  actionButton('button1',label='Here is a button')
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
  x <- seq(0,10,l=501)
  plot(dgamma(x,4,3), type='l', lwd=2, main='density plot of distance measure')
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
