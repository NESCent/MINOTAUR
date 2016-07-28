
##############
## MINOTAUR ##
##############


###################
## DOCUMENTATION ##
###################

#' Run the MINOTAUR app!
#'
#' Running this function will open the MINOTAUR app, a
#' shiny application that can be used to explore data, compare variables,
#' and identify outliers in multivariate and univariate space.
#'
#' @author Caitlin Collins \email{caitlin.collins12@@imperial.ac.uk}
#' @examples
#' \dontrun{
#' MINOTAUR()
#' }
#' @export
#' @importFrom shiny runApp
# @import shinydashboard
# @import shinyjs
# @import RColorBrewer
# @import ggplot2
# @import data.table
# @import Rcpp

########################################################################


MINOTAUR <- function(){
  .run.MINOTAUR()
  return(invisible())
} # end MINOTAUR


.run.MINOTAUR <- function(){
  # requireNamespace("shinydashboard")
## get path to App
  syst.file <- base::system.file
  filename <- syst.file("mainserver", package = "MINOTAUR")
  ## run app
  shiny::runApp(filename, launch.browser = TRUE)
}




