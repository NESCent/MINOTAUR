

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
#source("mhtCirclePlot.R")
#source("mhtplot.R")
source("uiFunctions.R")

#### ------------------------------------------------------------------

shinyUI(
  bootstrapPage(
    navbarPage(      
    
   ###########
   ## TITLE ##
   ###########  
   strong("MINOTAUR"), windowTitle= "MINOTAUR",
                         
   ##################
   ## WELCOME PAGE ##
   ##################
   
   ## TO DO: 
   ## Improve layout
   ## Add images of our prettiest plots
   
  tabPanel("Welcome",
    h2("Welcome to the labyrinth!"),
    p('MINOTAUR is a program for detection and visualisation of outliers in multivariate space'),
    div(img(src="minotaur.jpg"))
  ),
  
  
  #######################
  ## CLEANUP DATA PAGE ##
  #######################
  
  tabPanel("Clean-up data",
    fluidRow(
      column(5,
             h3("Clean-up Data"),
             
          wellPanel(                                    
            p(strong("Use this page to subset large datasets before plotting."),  
              br(),
              "Do", strong("NOT"), "remove outliers by hand!",
              #br(),
              "(Outliers will be identified in the adjacent tabs.)"
              ),
            style = "background-color: #eaf0f4; border-color:#5b95c2; padding:10px")
          
            # p('Use this page to reduce a large data set down to a smaller subset ready for plotting.',
            #  strong('Do not attempt to remove outliers at this stage!'),
            #  'These will be identified and plotted in remaining screens'),
            #  style = "background-color: #eaf0f4; border-color:#5b95c2; padding:10px")
      )
    ),
    fluidRow(
      column(3,
        h4(strong('Update or save changes to data:')),
        uiOutput('updateButton'),
        hr(),
        h4(strong("Select columns to remove"), "*", strong(":")),
        uiOutput('selectColumns'),
        helpText("*To select", em("multiple"), "values, use SHIFT or CTRL.",
                 br(),
                 "(Note: CTRL can also be used to deselect all values)"), 
                  ## Need to define what behaviour --> different fns of CTRL
        hr(),
        h4(strong("Select a variable to filter"), "*", strong(":")),        
        uiOutput('filterVariable'),
        helpText("*For large datasets, this can take a moment..."),
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
          p("Warning: scatterplot is slow with > 50,000 (need to test) points.  Suggest smooth scatter or hexplot."),
          plotOutput("scatterplot1"),
          h4('Making the scatterplot'),
          p("To do: Add 'none' to variable to color by.  Add sliding bars for x-axis and y-axis.")
        )
      )
    ),
    
    ## MANHATTAN PLOT
    tabPanel("Manhattan Plot",
      sidebarLayout(
      
        # sidebar panel
        sidebarPanel(
          h3('Manhattan Plot'),
          p('The x axis needs chromosome and coordinates informaiton. Make sure "Chr", "BP" in the columns names of your data or "clean-up data" '), 
          br(),
          
          uiOutput('linearMH_y1Selection'),
          checkboxGroupInput("logy1Checkbox", "log(y-axis) ", choices = c("log2","log10"),inline = TRUE),
          br(),
          
          uiOutput('linearMH_p2Selection'),
          br(),

          textInput(inputId="linearmhtpcut", label="P cutoff for outliers", value = 0.002)
          #uiOutput('select_linearMH')
        ),
        
        # main panel
        mainPanel(
          plotOutput("LinearMHTplot", height="450px",width="800px")
        )
      )
    ),
    
    ## CIRCLE PLOT
    tabPanel("Circle Plot",
      sidebarLayout(
        # sidebar panel
        sidebarPanel(
          h3('Circle Plot'),
          p("Be patient, SLOW with > 50,000 points. Suggest smooth scatter or hexplot."),
          br(),
          p('The circle plot needs chromosome and coordinates. Make sure "Chr", "BP" in the columns names of your data and "clean-up data"'), 
          br(),
          
          uiOutput('circle1mh'),
          checkboxGroupInput("logV1Checkbox", "log (outer circle) ", choices = c("log2","log10"),inline = TRUE),
          br(),
          
          uiOutput('circle2mh'),
          checkboxGroupInput("logV2Checkbox", "log (inner circle)", choices = c("log2","log10"),inline = TRUE),          
          br(),

          textInput(inputId="pcut", label="P cutoff for outliers", value = 0.001)
          ),
        
        # main panel
        mainPanel(
          plotOutput("circleMHTplot", height="800px",width="800px")
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
