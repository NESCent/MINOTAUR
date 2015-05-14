
#' This is MANIPULATE.
#' This function calls the shiny app.
#'
#'
#' @author Caitlin Collins \email{caitlin.collins12@@imperial.ac.uk}
#' @export
#' @import shiny rCharts rHighcharts stats4 

## NOTE1: ALL PACKAGES ADDED TO @import MUST BE USED SOMEWHERE IN MANIPULATE
## NOTE2: PLEASE TELL CAITLIN IF YOU ADD OR REMOVE ANY PACKAGES TO/FROM @import AS OTHER CHANGES WILL NEED TO BE MADE


## all imports (in use): ## shiny rCharts rHighcharts 
## NOTE: rCharts and rHighcharts are devel versions from github...

## other potential imports (not in use): ## devtools googleVis ggvis htmlwidgets



################
## MANIPULATE ##
################
## hidden function - DAPC server
labyrinth <- function(){
  runApp(system.file("./mainserver",package="MINOTAUR"))
}

