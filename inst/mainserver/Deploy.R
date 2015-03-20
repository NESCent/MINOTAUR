
# Deploy.R (one of three scripts required to run any Rshiny app)

# Author: Bob Verity
# Date: 18/03/2015

# Purpose:
# Simply points to the folder that the other Rshiny files are stored in and runs
# them.

#### ------------------------------------------------------------------

# Choose Shiny path based on user name
userName = 'Bob'
#userName = 'caitiecollins'
#userName = 'Daren'
#userName = 'saraschaal'
#userName = 'liuyang'

shinyPath = switch(userName,
                   'Bob'='/Users/Bob/Dropbox/Bob/Work/Side Projects/MANIPULATE/inst/mainserver',
                   'caitiecollins' = '/Users/caitiecollins/MANIPULATE/inst/mainserver',
                   'saraschaal' = '/Users/saraschaal/Documents/Wake Forest/Lotterhos Lab/Hackathon/MANIPULATE/inst/mainserver',
                   'liuyang' = 'G:/MANIPULATE/inst/mainserver'
                   
)

# Load packages
require(shiny)
require(devtools)
require(rCharts)
require(stats4)
#install_github('rCharts','ramnathv')
#install_github('rHighcharts','metagraf')

# run Shiny app
runApp(shinyPath)

# Note that running a shiny app will keep the console busy. It is possible to
# edit the source code in this state - then save changes to your code and
# refresh your browser to see the changes immediately. However, if this slows
# your computer down too much simply press esc in console to close app.
