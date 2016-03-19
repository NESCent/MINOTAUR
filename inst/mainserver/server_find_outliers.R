
########################
## FIND OUTLIERS PAGE ##  ------------------------------------------------------------------------------------
########################

##############################################
## Box: Controls for Distance-Based Methods ##
##############################################

# box containing controls for distance-based measure plots
output$box_control_distanceBased <- renderUI({
  box(title="Distance-Based Outlier Detection", status="warning", solidHeader=TRUE, collapsible=TRUE, width=12,
      h3('Header'),
      p('some text in here also'),
      actionButton('button1',label='Here is a button')
  )
})

###############################
## Box: Distance-Based Plots ##
###############################

# box for distance-based measures (harmonic mean and nearest-neighbour)
output$box_plot_distanceBased <- renderUI({
  tabBox(title='Distance-Based Methods', width=12,
         tabPanel('Harmonic Mean Distance','',
                  plotOutput('plot2')
         ),
         tabPanel('Nearest-Neighbour Distance','',
                  p('bloo blah')
         )
  )
})

# example plot
output$plot2 <- renderPlot({
  plot(rnorm(1e3))
})

#############################################
## Box: Controls for Density-Based Methods ##
#############################################

# box containing controls for density-based measure plots
output$box_control_densityBased <- renderUI({
  box(title="Density-Based Outlier Detection", status="warning", solidHeader=TRUE, collapsible=TRUE, width=12,
      p('this is where I put the details of density-based measures, such as kernel-density with fixed and ML bandwidth.')
  )
})

##############################
## Box: Density-Based Plots ##
##############################

# box for density-based measures (kernel density)
output$box_plot_densityBased <- renderUI({
  tabBox(title="Density-Based Methods", width=12,
         tabPanel('Heuristic Bandwidth','',
                  plotOutput('plot3')
         ),
         tabPanel('Maximum-Likelihood Bandwidth','',
                  p('begin trick')
         )
  )
})

# example plot
output$plot3 <- renderPlot({
  plot(rnorm(1e3))
})

