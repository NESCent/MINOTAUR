
###########################
## COMPARE OUTLIERS PAGE ##  ------------------------------------------------------------------------------------
###########################

###################################################
## Box:  ##
###################################################

#
output$box_compare1 <- shiny::renderUI({
  box(title="Compare", status="primary", solidHeader=TRUE, collapsible=FALSE, width=12, height=480,
      p('Not exactly sure what will go here, but some way of comparing the outliers identified by different variables.'),
      p('For example, perhaps a matrix showing the number of outliers that each compound measure has in common.'),
      p('Another idea would be to have a Venn diagram showing the same thing.')
  )
})

######################################
## Box:  ##
######################################

#
output$box_compare2 <- shiny::renderUI({
  box(title="Compare", status="warning", solidHeader=TRUE, collapsible=FALSE, width=12, height=480,
      h3('(something)')
  )
})
