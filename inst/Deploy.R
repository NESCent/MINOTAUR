
# Deploy.R (one of three scripts required to run any Rshiny app)

# Author: Bob Verity
# Date: 17/03/2015

# Purpose:
# Simply points to the folder that the other Rshiny files are stored in, and executes these files.

#### ------------------------------------------------------------------



# Choose Shiny path based on user name
# userName = 'Bob'
# 
# userName = 'Daren'
# 
# userName = 'caitiecollins'
# 
# userName = 'saraschaal'


if (userName=='Bob') {
  shinyPath = '/Users/Bob/Dropbox/Bob/Work/Side Projects/MANIPULATE/inst/'
}
if (userName=='caitiecollins') {
  shinyPath = '/Users/caitiecollins/MANIPULATE/inst/'
}

if (userName=='Bob') {
  shinyPath = '/Users/Bob/Dropbox/Bob/Work/Side Projects/MANIPULATE/inst/'
}

if (userName=='saraschaal'){
  shinyPath = '/Users/saraschaal/Documents/Wake Forest/Lotterhos Lab/Hackathon/MANIPULATE/inst/'
}

# Load packages
require("shiny")
require("devtools")
require("rCharts")
require("stats4")

# maybe needed?
#install_github('rCharts','ramnathv')
#install_github('rHighcharts','metagraf')

# run Shiny app
runApp(shinyPath)

