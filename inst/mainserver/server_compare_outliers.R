
###########################
## COMPARE OUTLIERS PAGE ##  ------------------------------------------------------------------------------------
###########################

###################################################
## Box:  ##
###################################################
#' @importFrom shinydashboard box
#' @importFrom shiny p
#' @importFrom shiny h3
#' @importFrom shiny renderUI
#
output$box_compare1 <- shiny::renderUI({
  shinydashboard::box(
    title="Compare", status="primary", solidHeader=TRUE, collapsible=FALSE, width=12, height=480,
    shiny::p('Not exactly sure what will go here, but some way of comparing the outliers identified by different variables.'),
    shiny::p('For example, perhaps a matrix showing the number of outliers that each compound measure has in common.'),
    shiny::p('Another idea would be to have a Venn diagram showing the same thing.')
  )
})

######################################
## Box:  ##
######################################

#
output$box_compare2 <- shiny::renderUI({
  shinydashboard::box(
    title="Compare", status="warning", solidHeader=TRUE, collapsible=FALSE, width=12, height=480,
    shiny::h3('(something)')
  )
})
