

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
# source("uiFunctions.R")

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
               div(img(src="minotaur.jpg", align="right")),
               h2("Welcome to the labyrinth!"),
               p('MINOTAUR ("MultIvariate Nifty OuTlier Analysis Using R") is a program for detection and visualisation of outliers in multivariate space. Although
                 this App has been designed with genomic data in mind, any dataset being analyzed in multivariate space
                 can be visualized using our App.'),
               h2('App Navigation:'),
               h3('1. Data'),
               p('MINOTAUR is supplied with example datasets for users to explore the functions and plotting capabilities of the RShiny App.'),
               h4('largeData'),
               p('{details about this data set}'),
               h4('smallData'),
               p('{details about this data set}'),
               br(),
               p('The user can also upload their own data sets as either .Rdata or Excel files following the example format below:'),
               div(img(src="example_dataframe.jpg", align="center")),
               p('rows can be individuals or for genomic data an example would be SNPs'),
               p('columns are all statstics (e.g. Fst, GWAS, etc.) measurements (e.g. environmental or phenotypic variables), or descriptors (e.g. chromosome, species, etc.)'),
               br(),
               br(),
               h4('Clean up Data Page'),
               p('This page allows you to subset your data by either removal of some variables (columns) or
                 by filtering your data based on a variable.'),
               p('Once you select a column to filter by, an additional menu will pop up that gives you either a
                 sliding bar to subset values of numerical data (left image below) or a box to remove levels of factor data
                 (right image below). A violin plot will appear for continuous numerical data.'),
               div(img(src="numerical_example.jpg", align = "center"), img(src="factor_example.jpg", align = "center")),
               h3('2. Plots'),
               h4('Scatterplot'),
               br(),
               h4('Manhattan Plot'),
               p('Used for genomic data to visualize outliers loci relative to their chromosomal location.'),
               br(),
               h4('Circle Plot'),
               br(),
               h3('3. Help Page'),
               p('Additional help including common mistakes or questions for using this App.')


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
                              #p('Please choose chromosome and coordinates informaiton for x axis'),
                              #style = "background-color: #eaf0f4; border-color:#5b95c2; padding:10px"),
                              #br(),
                              p('Choose x-axis'),
                              uiOutput('linearMH_xchr'),
                              uiOutput('linearMH_xcood'),
                              style = "background-color: #eaf0f4; border-color:#5b95c2; padding:10px"),
                              br(),

                              wellPanel(
                              p('Choose y-axis'),
                              uiOutput('linearMH_y1Selection'),
                              radioButtons("logy1Checkbox", "log(y-axis)", choices = c("log2","log10", "none"),inline = TRUE,selected = "none"),
                              radioButtons("flipY", "Flip Y", choices = c("Yes", "No"),inline = TRUE,selected = "No"),
                              textInput(inputId="linearmht_nbins", label="Number of bins", value = 100),
                              style = "background-color: #eaf0f4; border-color:#5b95c2; padding:10px"),
                              br(),

                              wellPanel(
                              uiOutput('linearMH_p2Selection'),
                              textInput(inputId="linearmhtpcut", label="P cutoff for outliers", value = 0.002),
                              style = "background-color: #eaf0f4; border-color:#5b95c2; padding:10px",
                              br())
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
                              HTML("Please be patient, this plot may be <strong>SLOW</strong> with > 50,000 points. For datasets too large for the circle plot, try using the scatterplot or Manhattanplot instead."),
                              style = "background-color: #fff; color: darkblue; border-color: white; padding:10px"),
                              br(),

                              wellPanel(
                                p('Choose x-axis'),
                                uiOutput('circle_xchr'),
                                uiOutput('circle_xcood'),
                                style = "background-color: #eaf0f4; border-color:#5b95c2; padding:10px"),
                              br(),

                              wellPanel(
                              uiOutput('circle1mh'),
                              radioButtons("logV1Checkbox", "log (outer circle) ", choices = c("-log2","-log10","none"),
                                           inline = TRUE,selected = "none"),
                              style = "background-color: #eaf0f4; border-color:#5b95c2; padding:10px"),
                              br(),

                              wellPanel(
                              uiOutput('circle2mh'),
                              radioButtons("logV2Checkbox", "log (inner circle)", choices = c("-log2","-log10","none"),
                                           inline = TRUE,selected = "none"),
                              style = "background-color: #eaf0f4; border-color:#5b95c2; padding:10px"),

                              br(),

                              textInput(inputId="pcut", label="P cutoff for outliers", value = 0.001)
                            ),

                            # main panel
                            mainPanel(
                              plotOutput("circleMHTplot", height="800px",width="800px")
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
