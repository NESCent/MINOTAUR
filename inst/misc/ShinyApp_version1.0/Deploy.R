
# Deploy.R (one of three scripts required to run any Rshiny app)

# Author: Bob Verity
# Date: 17/03/2015

# Purpose:
# Simply points to the folder that the other Rshiny files are stored in, and executes these files.

#### ------------------------------------------------------------------

# Choose Shiny path based on user name
userName = 'Bob'
<<<<<<< Updated upstream
#userName = 'Daren'
=======
userName = 'Daren'
userName = 'saraschaal'
>>>>>>> Stashed changes

if (userName=='Bob') {
  shinyPath = '/Users/Bob/Dropbox/Bob/Work/Side Projects/MANIPULATE/inst/misc/ShinyApp_version1.0'
}
if (userName=='Daren') {
  shinyPath = ''
}

if (userName=='saraschaal'){
  shinyPath = '/Users/saraschaal/Documents/Wake Forest/Lotterhos Lab/Hackathon/MANIPULATE/inst/misc/ShinyApp_version1.0'
}

# Load packages
require('shiny')
require('devtools')
require('rCharts')

# maybe needed?
#install_github('rCharts','ramnathv')
#install_github('rHighcharts','metagraf')

# run Shiny app
runApp(shinyPath)

