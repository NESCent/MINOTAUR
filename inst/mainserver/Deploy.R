
# Deploy.R (one of three scripts required to run any Rshiny app)

# Author: Bob Verity
# Date: 18/03/2015

# Purpose:
# Simply points to the folder that the other Rshiny files are stored in and runs
# them.

#### ------------------------------------------------------------------

# Choose Shiny path based on user name
#userName = 'Bob'
#userName = 'caitiecollins'
userName = 'Daren'
#userName = 'saraschaal'
userName = 'liuyang'
#userName = 'katie'
#userName = 'Nik'

shinyPath = switch(userName,
                   'Bob'='/Users/Bob/Dropbox/Bob/Work/Side Projects/MINOTAUR/inst/mainserver',
                   'caitiecollins' = '/Users/caitiecollins/MINOTAUR/inst/mainserver',
                   'saraschaal' = '/Users/saraschaal/Documents/Wake Forest/Lotterhos Lab/Hackathon/MINOTAUR/inst/mainserver',
                   'liuyang' = 'G:/MINOTAUR/inst/mainserver',
                   'Daren' = '/Users/darencard/Desktop/git/MINOTAUR/inst/mainserver',
                   'katie' = '~/Desktop/MINOTAUR/inst/mainserver',
                   'Nik' = '~/Documents/Niks_office/Scratch/MINOTAUR/inst/mainserver'
)

# Load packages
if(!('shiny' %in% installed.packages())){
  install.packages("shiny")
}
if(!('devtools' %in% installed.packages())){
  install.packages('devtools')
}
if(!('stats4' %in% installed.packages())){
  install.packages('stats4')
}
if(!('scales' %in% installed.packages())){
  install.packages('scales')
}

if(!('rCharts' %in% installed.packages())){
  install_github('rCharts')
}
if(!('ramnathv' %in% installed.packages())){
  install_github('ramnathv')
}
if(!('rHighcharts' %in% installed.packages())){
  install_github('rHighcharts')
}
if(!('metagraf' %in% installed.packages())){
  install_github('metagraf')
}
if(!('adegenet' %in% installed.packages())){
  install.packages('adegenet')
}

require(devtools)
require(rCharts)
require(stats4)
require(scales)
require(adegenet)
require(shiny)

# run Shiny app
runApp(shinyPath)

# Note that running a shiny app will keep the console busy. It is possible to
# edit the source code in this state - then save changes to your code and
# refresh your browser to see the changes immediately. However, if this slows
# your computer down too much simply press esc in console to close app.
