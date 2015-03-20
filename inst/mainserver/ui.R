
##########
## ui.R ##
##########

# This is the user interface.
# This function is the pretty face of our shiny app.


# temporarily loading all required packages here
require("shiny")
require("rCharts")
require("rHighcharts")
require("stats4")
require("adegenet")

## temporarily sourcing .R files here
source("mhtCirclePlot.R")
source("mhtplot.R")


#### ------------------------------------------------------------------

shinyUI(navbarPage("MINOTAUR",
                   
                   tabPanel("Welcome",
                            h2("hello world")
                   ),
                   
                   tabPanel("Cleanup data",
                            fluidRow(
                              column(3,
                                     uiOutput('filterVariable'),
                                     helpText('choose a variable to subset by'),
                                     uiOutput('filterActiveDefault'),
                                     htmlOutput('filterOptions'),
                                     uiOutput('subsetPanels')
                              ),
                              column(8,
                                     dataTableOutput("mainDataTable")
                              )
                            )
                   ),
                   
                   tabPanel("Produce plots",
                            fluidRow(
                              column(4,
                                     h2('Fancy Plots'),
                                     uiOutput('selectplot'),
                                     p('Select a proper statistic for plotting')
                              ),
                              column(6,
                                     showOutput("test1", "Highcharts"),
                                     plotOutput("circleMHTplot", height="400px",width="400px"),
                                     plotOutput("LinearMHTplot", height="400px",width="600px")
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
