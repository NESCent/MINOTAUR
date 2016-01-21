

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
      
      ##############
      ## DATA TAB ##
      ##############
      
      navbarMenu("Data",
                 
                 ######################
                 ## INPUT DATA PAGE ##
                 ######################
                 
                 tabPanel("Input data",                     
                          sidebarLayout(
                            ## sidebar Panel
                            sidebarPanel(
                              h3("Data selection"),
                              
                              hr(),
                              
                              ## define the type of input ##
                              radioButtons("datatype", HTML("<h4><strong>What data source to use?</strong></h4>"),
                                           list("Example from MINOTAUR"="eg",
                                                "Upload data frame as .Rdata"="file",
                                                "Upload Excel spreadsheet"="csv")),
                              hr(),
                              
                              ## choice of dataset if source is an example ##
                              conditionalPanel(condition = "input.datatype=='eg'",
                                               selectInput("egData", HTML("<h4><strong>Select an example dataset:</strong></h4>"),
                                                           choices=c("smallData", 
                                                                     "largeData"),
                                                           selected = "largeData"),
                                               br(),br()
                              ),
                              
                              ## choice of dataset if source is a .Rdata file (containing a data frame!) ##
                              conditionalPanel(condition = "input.datatype=='file'",
                                               fileInput("fileData", HTML("<h4><strong>Browse to upload .Rdata file*</strong></h4>"))  
                                               ,
                                               helpText("*This option currently supports only data frames saved as .Rdata")
                              )
                              ,
                              
                              ## choice of dataset if source is an Excel file ##
                              conditionalPanel(condition = "input.datatype=='csv'",
                                               fileInput("csvData", HTML("<h4><strong>Browse to upload Excel spreadsheet*</strong></h4>"))  
                                               ,
                                               helpText("*Supported Excel file extensions are: .csv, .xls, .xlsx")
                              ),
                              hr()
                            ),
                            
                            ## Summary - main panel ##
                            mainPanel(                              
                              h3("Summary"),
                              
                              ## print summary of input data:
                              verbatimTextOutput("inputSummary")
                              
                            )                                               
                            
                          ) # end sidebarLayout                  
                 ), # end Input data page
                 
                 #######################
                 ## CLEANUP DATA PAGE ##
                 #######################
                 
                 tabPanel("Clean-up data",
                          
                          sidebarLayout(
                            
                            # sidebar panel
                            sidebarPanel(
                              h3("Clean-up Data"),
                              
                              wellPanel(                                    
                                p(strong("Use this page to subset large datasets before plotting."),  
                                  br(),
                                  "Do", strong("NOT"), "remove outliers by hand!",
                                  #br(),
                                  "(Outliers will be identified in the adjacent tabs.)"
                                ),
                                style = "background-color: #eaf0f4; border-color:#5b95c2; padding:10px")
                              ,
                              
                              h4(strong('Update or save changes to data:')),
                              uiOutput('updateButton'),
                              br(),
                              wellPanel(
                                HTML(" Note that you must press 'Update' after <i><strong>each</strong></i> change made below."),
                                style = "background-color: #eaf0f4; border-color:#5b95c2; padding:4px"),
                              hr(),
                              h4(strong("Select columns to remove"), "*", strong(":")),
                              uiOutput("selectColumns"),
                              helpText("*To select", em("multiple"), "values, use SHIFT or CTRL.",
                                       br(),
                                       "(Note: CTRL can also be used to deselect all values)"), 
                              ## Need to define what behaviour --> different fns of CTRL
                              hr(),
                              h4(strong("Select a variable to filter"), "*", strong(":")),        
                              uiOutput("filterVariable"),
                              helpText("*For large datasets, this can take a moment..."),
                              uiOutput('subsetPanels_current'),
                              hr(),
                              uiOutput("subsetPanels_locked")
                            ),
                            
                            mainPanel(
                              dataTableOutput("mainDataTable")
                              #,htmlOutput("scratchPad")
                            )
                          )
                 ) # end Clean-up data page
                 
      ), # end Data navbarMenu
      
      
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
                              wellPanel(
                              p("Change axes to different outlier statistics to compare between them."),
                              style = "background-color: #eaf0f4; border-color:#5b95c2; padding:10px"),
                              br(),
                              
                              uiOutput("scatter_xSelection"),
                              checkboxGroupInput("scatter_Checkbox_x", "log(x-axis) base (check one)", 
                                                 choices = c("2","10"),inline = TRUE),
                              uiOutput("scatter_ySelection"),
                              checkboxGroupInput("scatter_Checkbox_y", "log(y-axis) base (check one)", 
                                                 choices = c("2","10"),inline = TRUE, select="2"),
                              
                              textInput(inputId="scatter_nbins", label="Number of bins", value = 100),
                              
                              uiOutput("scatter_colVarSelection"),
                              textInput(inputId="scatter_cutoff", 
                                        label="Cutoff for outliers to overlay.  NEED TO MAKE A CHECK BOX FOR UPPER OR LOWER TAIL. (if blank: default lower 1% tail of 2nd variable chosen).  
                                        Would be cool to make this violin plot...)", value = NULL)#,
                              
                              #uiOutput("scatter_colPal")
                            ),
                            
                            # main panel
                            mainPanel(
                              h3('Scatterplot'),
                              p("Warning: this plot is for continuous variables. Do not plot factors."),
                              plotOutput("scatterplot1"),
                              h4('Making the scatterplot'),
                              p("First, choose x and y variables to plot.  
                                Next, you can overlay points in the plot according to a third variable of your choice.
                                By default, the lower 1% of the y-axis variable will be plotted.
                                For example, choose 'Trait3_p' to see which outliers in Trait3 are also outliers in Trait1."
                              ),
                              p("To do: (1) Add zoom sliding bars for x-axis and y-axis 
                                (2) Add flip options for x and y axis log-axis option
                                (2.5)  Make friendly to new datasets (choose numeric)
                                (3) Can we get the mouse to tell us the name of a point (!)."),
                              h4("Outliers"),
                              p("The table below lists the outliers: the data points below (or above, depending on your selection) 
                                the threshold chosen above for 'Cutoff for outliers to overlay'."
                              ),
                              dataTableOutput("scatterDataTable")
                              )
                              )
                              ),
                 
                 ## MANHATTAN PLOT
                 tabPanel("Manhattan Plot",
                          sidebarLayout(
                            
                            # sidebar panel
                            sidebarPanel(
                              h3('Manhattan Plot'),
                              wellPanel(
                              p('The x axis needs chromosome and coordinates informaiton. 
                                Make sure "Chr", "BP" in the columns names of your data or "clean-up data" '),
                              style = "background-color: #eaf0f4; border-color:#5b95c2; padding:10px"),
                              br(),
                              
                              uiOutput('linearMH_y1Selection'),
                              radioButtons("logy1Checkbox", "log(y-axis)", choices = c("log2","log10", "none"),inline = TRUE,selected = "none"),
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
                              wellPanel(
                              HTML("Please be patient, this plot may be <strong>SLOW</strong> with > 50,000 points."),
                              br(),
                              p("For datasets too large for the circle plot, try using the smooth scatter or hexplot instead."),                              
                              br(),
                              p('The circle plot needs chromosome and coordinates. 
                                Make sure "Chr", "BP" in the columns names of your data and "clean-up data"'), 
                              style = "background-color: #eaf0f4; border-color:#5b95c2; padding:10px"),
                              br(),
                              hr(),
                              uiOutput('circle1mh'),
                              radioButtons("logV1Checkbox", "log (outer circle) ", choices = c("log2","log10","none"),
                                           inline = TRUE,selected = "none"),
                              br(),
                              hr(),
                              uiOutput('circle2mh'),
                              radioButtons("logV2Checkbox", "log (inner circle)", choices = c("log2","log10","none"),
                                           inline = TRUE,selected = "none"),          
                              br(),
                              hr(),
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
