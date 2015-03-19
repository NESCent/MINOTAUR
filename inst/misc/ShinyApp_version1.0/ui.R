
# ui.R (one of three scripts required to run any Rshiny app)

# Author: Bob Verity
# Date: 18/03/2015

# Purpose:
# Defines the user interface, including any controls that the user interacts
# with. Interactive objects from within this script pass values to the system
# script that can be used as ordinary variables.

#### ------------------------------------------------------------------

shinyUI(navbarPage("MANIPULATE",
                   
  tabPanel("Welcome",
    h2("hello world")
  ),
  
  tabPanel("Cleanup data",
    fluidRow(
      column(3,
        uiOutput('filterVariable'),
        uiOutput('filterActiveDefault'),
        htmlOutput('filterActive'),
        htmlOutput('filterOptions'),
        helpText('here is some help text')
      ),
      column(8,
        dataTableOutput("mainDataTable")
      )
    )
  ),
  
  tabPanel("Produce plots",
    fluidRow(
      column(4,
        h2('booboo'),
        p('here is some text')
      ),
      column(6,
        showOutput("test1", "Highcharts")
      )
    )
  ),
  
  navbarMenu("More",
    tabPanel("(sub-menu A)",
      tableOutput("summaryTable")
    ),
    tabPanel("(sub-menu B)",
      plotOutput("randomHist",height="300px",width="300px")
    )
  )
  
))
