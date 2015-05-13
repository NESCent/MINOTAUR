

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

shinyUI(bootstrapPage(navbarPage("MINOTAUR",
   
                   
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
      column(5,
          wellPanel(
            p('Use this page to reduce a large data set down to a smaller subset ready for plotting.',strong('Do not attempt to remove outliers at this stage!'),'These will be identified and plotted in remaining screens'),
            style = "background-color: #eaf0f4; border-color:#5b95c2; padding:10px")
      )
    ),
    fluidRow(
      column(3,
        h5('update or save changes to data'),
        uiOutput('updateButton'),
        hr(),
        h5('select columns to remove'),
        uiOutput('selectColumns'),
        helpText('use shift or ctrl to select multiple values (ctrl can also be used to deselect all values)'),
        hr(),
        h5('select variable to filter'),
        uiOutput('filterVariable'),
        uiOutput('subsetPanels_current'),
        hr(),
        uiOutput('subsetPanels_locked')
      ),
      column(8,
        dataTableOutput("mainDataTable")
        #,htmlOutput('scratchPad')
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
      sidebarLayout(
          
        # sidebar panel
        sidebarPanel(
            
          # header and information
          h2("Comparing Statistics"),
          p("Change axes to different outlier statistics to compare between them."),
          br(),
            
          uiOutput("hex_xSelection"),
          uiOutput("hex_ySelection"),
          uiOutput("hex_colVarSelection"),
          uiOutput("hex_colPal")
        ),
      
        # main panel
        mainPanel(
          h3('Hex plot (plain R)'),
          plotOutput("hexChart")
      )
    )
  ),
  
    # SMOOTHSCATTER PLOT
    tabPanel("Smoothed Scatter Plot",
      sidebarLayout(
             
        # sidebar panel
        sidebarPanel(
               
          # header and information
          h2("Comparing Statistics"),
          p("Change axes to different outlier statistics to compare between them."),
          br(),
       
          uiOutput("smooth_xSelection"),
          uiOutput("smooth_ySelection"),
          uiOutput("smooth_colVarSelection"),
          uiOutput("smooth_colPal")
          ),
             
      # main panel
      mainPanel(
        h3('Smoothed Scatter Plot'),
        plotOutput("smoothscatter")
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
  
)))