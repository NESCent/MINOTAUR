
##############
## MINOTAUR ##
##############

############################################################################
## ALTERNATIVE WAY TO INSTALL THE MOST UP-TO-DATE VERSION OF THE PACKAGE: ##
############################################################################
## (Solution to "Warning invalid package MINOTAUR Error ERROR no package specified".)

# install.packages("devtools", dependencies=TRUE)
# library(devtools)
# install_github("Bioconductor-mirror/OmicCircos", ref="5d2b5b2") # ref="5d2b5b2" # OLD: # ref="b772950"
# install_github("rstudio/DT", ref="24d71f2")
# install_github("NESCent/MINOTAUR", build_vignettes=TRUE)
# library(MINOTAUR)
# MINOTAUR()

# ERROR: dependencies 'DT', 'OmicCircos', 'shinydashboard', 'shinyjs' are not available for package 'MINOTAUR'

#library(devtools); devtools::check()



#####

## vignette?
# vignette("MINOTAUR")
# devtools::install(build_vignettes = TRUE)

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
#' @author Caitlin Collins \email{caitlin.collins12@@imperial.ac.uk}
#' @examples
#'
#' ## run function
#' MINOTAUR()
#'
#'
#' @import shiny shinydashboard shinyjs DT
#'  adegenet  RColorBrewer
#'  ggplot2 data.table ash fields Rcpp
#'  OmicCircos
#'
#' @export

########################################################################

# importFrom(fields,flame)
# importFrom(shiny,dataTableOutput)
# importFrom(shiny,renderDataTable)
# importFrom(shinyjs,runExample)
# importFrom(shinyjs,show)

# ## WORKING:
# #' @import shiny shinydashboard shinyjs DT
# #'  adegenet  RColorBrewer
# #'  ggplot2 data.table ash fields Rcpp
# #'  OmicCircos
#
# #'  @importFrom fields flame
# #'  @importFrom shiny dataTableOutput renderDataTable
# #'  @importFrom shinyjs runExample show

# Rcpp
# useDynLib(RgeoProfile)
# exportPattern("^[[:alpha:]]+")
# importFrom(Rcpp, evalCpp)

## OmicCircos ## this works...
# library(devtools)
# install_github("Bioconductor-mirror/OmicCircos", ref="b772950")
# require(OmicCircos)
# install_github("rstudio/DT", ref="24d71f2")
# require(DT)


## devtools fns ##
# document() # adds imports to NAMESPACE and doc to Rd files (for all saved .R files)
# use_package("package") # adds imports to DESCRIPTION
# imports <- c("shiny", "shinydashboard", "shinyjs", "adegenet", "RColorBrewer", "ggplot2", "data.table", "ash", "fields", "Rcpp")
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


  ## handling check error: library or require calls not declared from [all imported pkgs]

  ## get list of dependencies:
  pkgs <- list("shiny",
               "shinydashboard",
               "shinyjs",
               "adegenet",
               "RColorBrewer",
               "ggplot2",
               "data.table",
               "ash",
               "fields",
               "Rcpp")
  ## require/install/load pks:
  for(i in 1:length(pkgs)){
    .dynamic_require(pkgs[[i]])
  }
  ## require/install_GITHUB/load DT, OmicCircos:
  .dynamic_require_DT("DT")
  .dynamic_require_OmicCircos("OmicCircos")

  #   requireNamespace("shiny")
  #   requireNamespace("shinydashboard")
  #   requireNamespace("shinyjs")
  #   requireNamespace("DT")
  #   requireNamespace("adegenet")
  #   requireNamespace("RColorBrewer")
  #   requireNamespace("ggplot2")
  #   requireNamespace("data.table")
  #   requireNamespace("ash")
  #   requireNamespace("fields")
  #   requireNamespace("Rcpp")
  #   requireNamespace("OmicCircos")

  #   require(shiny)
  #   require(shinydashboard)
  #   require(data.table)
  #   require(shinyjs)
  #   # require(Hmisc) # only for color table demo -- may NOT need!
  #   require(adegenet)
  #   require(ash)
  #   require(fields)
  #   require(data.table)
  #   require(ggplot2)
  #   require(RColorBrewer)
  #   require(Rcpp)

  # require(devtools)
  # install_github("Bioconductor-mirror/OmicCircos", ref="b772950")
  # require(OmicCircos)
  # install_github("rstudio/DT", ref="24d71f2")
  # require(DT)


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

#################################
## .dynamic_require_OmicCircos ##
#################################
.dynamic_require_OmicCircos <- function(package="OmicCircos"){
  if(eval(parse(text=paste("require(",
                           package,
                           ", quietly=TRUE)")))) return(TRUE)

  .dynamic_require("devtools")
  install_github("Bioconductor-mirror/OmicCircos", ref="5d2b5b2")
  return(eval(parse(text=paste("require(",
                               package,
                               ", quietly=TRUE)"))))
} # .dynamic_require_OmicCircos

#########################
## .dynamic_require_DT ##
#########################
.dynamic_require_DT <- function(package="DT"){
  if(eval(parse(text=paste("require(",
                           package,
                           ", quietly=TRUE)")))) return(TRUE)

  .dynamic_require("devtools")
  install_github("rstudio/DT", ref="24d71f2")
  return(eval(parse(text=paste("require(",
                               package,
                               ", quietly=TRUE)"))))
} # .dynamic_require_DT

######################
## .dynamic_require ##
######################
.dynamic_require <- function(package){
  if(eval(parse(text=paste("require(",
                           package,
                           ", quietly=TRUE)")))) return(TRUE)

  install.packages(package,
                   dependencies=TRUE,
                   quiet=TRUE,
                   verbose=FALSE)
  return(eval(parse(text=paste("require(",
                               package,
                               ", quietly=TRUE)"))))
} # end .dynamic_require
