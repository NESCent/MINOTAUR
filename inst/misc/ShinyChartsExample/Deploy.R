
# Deploy.R (one of three scripts required to run any Rshiny app)

# Author: Bob Verity
# Date: 02/04/2014

# Purpose:
# Simply points to the folder that the other Rshiny files are stored in, and runs them.

#### ------------------------------------------------------------------

# Load packages
#loadPackage('shiny')
#loadPackage('devtools')
#install_github('rCharts','ramnathv')
#require(rCharts)

#needed? install_github('rHighcharts','metagraf')
#require(rHighcharts)

# run Shiny app
runApp('G:/MANIPULAT/inst/misc/ShinyChartsExample')

# Note that running a shiny app will keep the console busy. It is possible to edit the source code in this state - then save changes to your code and refresh your browser to see the changes immediately. However, if this slows your computer down too much simply press esc in console to close app.
