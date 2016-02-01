
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
  
  ## TO DO: FIND A FUNCTIONAL SOLUTION TO THE "CONNECTION" WARNING!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!11
  ## Repetitive warnings being caused by conflicting 
  ## definitions of class "connection" (pkgs BiocGeneric & RJSONIO)
  ## Fortunately we do not need this class from either pkg.
  #connection <- setClass("connection", slots = c(x="numeric", y="numeric")) ## not resolving warning...
  
  require(shiny) 
  require(rCharts) 
  require(rHighcharts) 
  require(stats4) 
  require(adegenet) 
  require(MASS) 
  require(RColorBrewer) 
  require(ggplot2) 
  require(scales) 
  require(hexbin) 
  require(OmicCircos) 
  require(data.table) 
  require(ash) 
  require(fields) 
  
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

