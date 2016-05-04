
##############
## MINOTAUR ##
##############

############################################################################
## ALTERNATIVE WAY TO INSTALL THE MOST UP-TO-DATE VERSION OF THE PACKAGE: ##
############################################################################
## (Solution to "Warning invalid package MINOTAUR Error ERROR no package specified".)

# install.packages("devtools", dependencies=TRUE)
# library(devtools)
# install.packages("OmicCircos", dependencies=TRUE)
# library(OmicCircos)
# install_github("NESCent/MINOTAUR")
# library(MINOTAUR)
# MINOTAUR()

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
#'
#' @import shiny shinydashboard shinyjs DT
#'  adegenet  RColorBrewer
#'  ggplot2 hexbin  data.table ash fields Rcpp
#'  Hmisc OmicCircos Rcpp
#'
#'
#' @export

########################################################################

## WORKING:
# #' @import shiny shinydashboard shinyjs DT
# #'  stats4 adegenet  MASS RColorBrewer
# #'  ggplot2 scales hexbin  data.table ash fields Rcpp
# #'  Hmisc OmicCircos Rcpp
# f


## PKG LIST (28/04/2016): ##
# adegenet ash data.table DT
# fields ggplot2 hexbin Hmisc
# MASS OmicCircos RColorBrewer Rcpp
# scales shiny shinydashboard shinyjs stats4

# #' @import adegenet ash data.table DT
# #' fields ggplot2 Hmisc
# #' OmicCircos RColorBrewer Rcpp
# #' shiny shinydashboard shinyjs
# f

# Rcpp
# useDynLib(RgeoProfile)
# exportPattern("^[[:alpha:]]+")
# importFrom(Rcpp, evalCpp)

## OmicCircos ## this works...
# library(devtools)
# install_github("Bioconductor-mirror/OmicCircos", ref="b772950")
# require(OmicCircos)

## devtools fns ##
# document() # adds imports to NAMESPACE and doc to Rd files (for all saved .R files)
# use_package("package") # adds imports to DESCRIPTION
# imports <- c("shiny", "stats4", "adegenet", "MASS", "RColorBrewer", "ggplot2", "scales", "hexbin", "data.table", "ash", "fields")
# for(i in (1:length(imports))){
#   use_package(imports[i])
# }

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
  require(shinydashboard)
  require(data.table)
  require(shinyjs)
  require(DT)
  require(Hmisc) # only for color table demo -- may NOT need!
  require(adegenet)
  require(ash)
  require(fields)
  require(data.table)
  require(ggplot2)
  require(OmicCircos)
  require(RColorBrewer)
  require(Rcpp)

  #   require(adegenet)
  #   require(ash) # .plot2D
  #   require(data.table)
  #   require(DT)
  #   require(fields) # .plot2D
  #   require(ggplot2)
  #   require(Hmisc) # only for color table demo -- may NOT need!
  #   require(OmicCircos)
  #   require(RColorBrewer)
  #   require(Rcpp)
  #   require(shiny)
  #   require(shinydashboard)
  #   require(shinyjs)
  # require(MASS) # NOT SURE IF USED -- REMOVED (unless someone needs it???)
  # require(scales) # NOT SURE IF USED -- REMOVED (unless someone needs it???)
  # require(stats4) # NOT SURE IF USED -- REMOVED (unless someone needs it???)


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
  runApp(filename, launch.browser=TRUE)
}

