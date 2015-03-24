
##############
## Server.R ##
##############

# This is the MINOTAUR server.
# This function calls all of the functions running behind our app.

# temporarily loading all required packages here
require("shiny")
require("rCharts")
require("rHighcharts")
require("stats4")
require("adegenet")
require("MASS")
require("RColorBrewer")
require("ggplot2")
require("scales")
require("hexbin")

## temporarily sourcing .R files here
source("mhtCirclePlot.R")
source("mhtplot.R")
source("uiFunctions.R")

#### ------------------------------------------------------------------

# load in the data (use mytoys.txt data for now)
mainData <- read.table("mytoys.txt",head=T)


# Define the shiny server functionality
shinyServer(function(input, output) {
  
  # Cleanup data
  source("server_cleanupData.R", local=T)
  
  # Scatterplot
  source("server_scatterplot1.R", local=T)
  
  # Hex plot 1 (plain R)
  source("server_hexplot1.R", local=T)
  
  # Hex plot 2 (ggplot)
  source("server_hexplot2.R", local=T)
  
  # Linear Manhattan plot
  source("server_Manhattanplot.R", local=T)
  
  # Circular Manhattan plot
  source("server_circleplot.R", local=T)
  
  # (example summary table)
  output$summaryTable <- renderTable({
    summary(mainData)
  })
  
})
