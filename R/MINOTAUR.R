
##############
## MINOTAUR ##
##############


########################################################################

###################
## DOCUMENTATION ##
###################

#' Run the MINOTAUR app!
#'
#' Running this function will open the MINOTAUR app, a
#' shiny application that can be used to explore data, compare variables, 
#' and identify outliers in multivariate and univariate space. 
#'
#' 
#' @author Caitlin Collins \email{caitiecollins@@gmail.com}
#' @examples
#'
#' ## run function
#' MINOTAUR()
#'
#' @import shiny rCharts rHighcharts stats4 adegenet  MASS RColorBrewer ggplot2 scales hexbin OmicCircos data.table ash fields 
#' 
#' @export 

########################################################################

MINOTAUR <- function(){
  .run.MINOTAUR()
  return(invisible())
} # end MINOTAUR


.run.MINOTAUR <- function(){
  syst.file <- base::system.file
  
  #   ## temporarily (?!) sourcing .R files here ##
  #   source(syst.file("mainserver/data.R",package="MINOTAUR"))
  #   source(syst.file("mainserver/server_cleanupData.R",package="MINOTAUR"))
  #   source(syst.file("mainserver/server_Manhattanplot.R",package="MINOTAUR"))
  #   source(syst.file("mainserver/server_circleplot.R",package="MINOTAUR"))
  #   source(syst.file("mainserver/uiFunctions.R",package="MINOTAUR"))
  #   source(syst.file("mainserver/utils.R",package="MINOTAUR"))
  
  #   syst.file("mainserver/data.R",package="MINOTAUR")
  #   source(syst.file("data.R",package="MINOTAUR"))
  
  ## get path to App
  filename <- syst.file("mainserver",package="MINOTAUR")
  ## run app
  runApp(filename)
}

