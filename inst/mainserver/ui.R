
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
source("uiFunctions.R")


#### ------------------------------------------------------------------

shinyUI(navbarPage("MINOTAUR",
                   
                   tabPanel("Welcome",
                            h2("Welcome to the labyrinth!"),
                            p('MINOTAUR is a program for detection and visualisation of outliers in multivariate space'),
                            div(img(src="minotaur.jpg"))
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
                   
                   navbarMenu("Produce plots", 
                              tabPanel("Stat Compare",
                                       sidebarLayout( 
                                         
                                         # Sidebar with Dropdown Menu
                                         sidebarPanel(
                                           
                                           #Header and information
                                           h2("Comparing Statistics"),
                                           p("Change axes to different outlier statistics to compare between them."),
                                           
                                           # Line Break
                                           br(),   
                                           
                                           uiOutput("ySelection"),
                                           uiOutput("xSelection"),
                                           uiOutput("reactiveui3"),
                                           uiOutput("col.pal"),
                                           
                                           br(),
                                           br()
                                           
                                           # Plots are update via this button
                                           #submitButton("Update Plots")
                                         ),
                                         
                                         # Plot on Main Panel      
                                         mainPanel(
                                           h3('Scatterplot'),
                                           plotOutput("bubbleChart1")
                                         )
                                       )
                              ),
                              tabPanel("Manhattan Plot",
                                       fluidRow(
                                         column(4,
                                                h3('Manhattan Plot'),
                                                uiOutput('selectplot_linearMH'),
                                                p('Select a proper statistic for plotting')
                                         ),
                                         column(6,
                                                plotOutput("LinearMHTplot", height="400px",width="600px")
                                         )
                                       )
                              ),
                              tabPanel("Circle Plot",
                                       fluidRow(
                                         column(4,
                                                h3('Circle Plot'),
                                                uiOutput('selectplot_circleMH'),
                                                p('Select a proper statistic for plotting')
                                         ),
                                         column(6,
                                          plotOutput("circleMHTplot", height="400px",width="400px")
                                         )
                                       )
                              ),
                              tabPanel("Hex Plot",
                                       fluidRow(
                                         mainPanel(
                                           h3('Hex plot'),
                                           plotOutput("hexChart")
                                         )
                                       )
                              ),
                              tabPanel("ggplot Hex Plot",
                                       fluidRow(
                                         mainPanel(
                                           h3('ggplot Hex plot'),
                                           plotOutput("ggHexChart")
                                         )
                                       )
                              ),
                              tabPanel("Contour Plot",
                                       fluidRow(
                                         mainPanel(
                                           h3('Contour plot'),
                                           plotOutput("contourChart")
                                         )
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
