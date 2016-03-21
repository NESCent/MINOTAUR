
#################################
## PLOT HISTOGRAM/DENSITY PAGE ##  ------------------------------------------------------------------------------------
#################################

#######################################################
## Box:  ##
#######################################################

#
output$box_1D_hist_controls <- renderUI({
  box(title="Histogram Controls", status="primary", solidHeader=TRUE, collapsible=FALSE, width=12,
      h3('Histogram Controls'),
      p('Controls for creating a 1D histogram plot, eg. breaks etc.')
  )
})

##########################
## Box:  ##
##########################

#
output$box_1D_hist_plot <- renderUI({
  box(title="Histogram", status="warning", solidHeader=TRUE, collapsible=FALSE, width=12,
      h3('Histogram')
  )
})
