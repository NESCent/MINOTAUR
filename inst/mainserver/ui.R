
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
                   
   ##################
   ## WELCOME PAGE ##
   ##################
   
  tabPanel("Welcome",
    h2("Welcome to the labyrinth!"),
    p('MINOTAUR is a program for detection and visualisation of outliers in multivariate space'),
    div(img(src="minotaur.jpg"))
  ),
  
  
  #######################
  ## CLEANUP DATA PAGE ##
  #######################
  
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
  
  
  ########################
  ## PRODUCE PLOTS PAGE ##
  ########################
  
  navbarMenu("Produce plots",
    
    ## SCATTERPLOT
    tabPanel("Scatterplot",
      sidebarLayout(
        
        # sidebar panel
        sidebarPanel(
          
          # header and information
          h2("Comparing Statistics"),
          p("Change axes to different outlier statistics to compare between them."),
          br(),
          
          uiOutput("scatter_xSelection"),
          uiOutput("scatter_ySelection"),
          uiOutput("scatter_colVarSelection"),
          uiOutput("scatter_colPal")
        ),
        
        # main panel
        mainPanel(
          h3('Scatterplot'),
          plotOutput("scatterplot1")
        )
      )
    ),
    
    ## MANHATTAN PLOT
    tabPanel("Manhattan Plot",
      sidebarLayout(
      
        # sidebar panel
        sidebarPanel(
          h3('Manhattan Plot'),
          uiOutput('selectplot_linearMH'),
          p('Select a statistic for plotting')
        ),
        
        # main panel
        mainPanel(
          h3('Manhattan Plot'),
          plotOutput("LinearMHTplot", height="400px",width="600px")
        )
      )
    ),
    
    ## CIRCLE PLOT
    tabPanel("Circle Plot",
      sidebarLayout(
        
        # sidebar panel
        sidebarPanel(
          h3('Circle Plot'),
          uiOutput('selectplot_circleMH'),
          p('Select a statistic for plotting')
        ),
        
        # main panel
        mainPanel(
          h3('Circle Plot'),
          plotOutput("circleMHTplot", height="400px",width="400px")
        )
      )
    ),
    
    ## HEX PLOT 1
    tabPanel("Hex Plot (plain R)",
      fluidRow(
        mainPanel(
          h3('Hex plot (plain R)'),
          plotOutput("hexChart")
        )
      )
    ),
    
    ## HEX PLOT 2
    tabPanel("Hex Plot (ggplot)",
      fluidRow(
        mainPanel(
          h3('Hex plot (ggplot)'),
          plotOutput("ggHexChart")
        )
      )
    )
  ),
  
  
  #################
  ## 'MORE' PAGE ##
  #################
  
  # (currently just a place-holder showing how to make a summary table)
  
  navbarMenu("More",
    tabPanel("(sub-menu A)",
      tableOutput("summaryTable")
    )
  )
  
))
