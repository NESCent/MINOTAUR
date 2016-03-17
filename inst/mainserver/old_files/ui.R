

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


## Adding useful links to our App (Welcome Page only?) ##
URL_list <- list("MINOTAUR" = "https://github.com/NESCent/MINOTAUR",
                 "NESCent" = "http://nescent.org/",
                 "Report a Bug" = "https://github.com/NESCent/MINOTAUR/issues/new",
                 "Contact the Developers" =
                   "mailto:caitiecollins@gmail.com,
                    r.verity@imperial.ac.uk,
                    k.lotterhos@neu.edu,
                    dcard@uta.edu,
                    schaal.s@husky.neu.edu,
                    wallacewly@gmail.com,
                    grunwaln@science.oregonstate.edu")

URL_MINOTAUR <- list('MINOTAUR' = "https://github.com/NESCent/MINOTAUR")
URL_NESCent <- list("NESCent" = "http://nescent.org/")
URL_Bug <- list("Report Bugs" = "https://github.com/NESCent/MINOTAUR/issues/new")
URL_Contact <- list("Contact Us" =
                      "mailto:caitiecollins@gmail.com,
                      r.verity@imperial.ac.uk,
                      k.lotterhos@neu.edu,
                      dcard@uta.edu,
                      schaal.s@husky.neu.edu,
                      wallacewly@gmail.com,
                      grunwaln@science.oregonstate.edu")


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
          #sidebarLayout(
          fluidPage(

            ######################
            ## MINOTAUR banner: ##
            ######################
            wellPanel(
            ## Main Panel 1:
            fluidRow(
              column(3,
                     div(img(src="minotaur.jpg"), align = "left")
                ),
              column(9,
                HTML("<h1 style='padding: 0px 10px;'><big><big><big><strong>MINOTAUR </strong></big></big></big></h1>
                      <h4 style='padding: 0px 10px;'>
                      &#9899;
                      <strong>M</strong>ult<strong>I</strong>variate
                      v<strong>I</strong>sualisatio<strong>N</strong>
                       and <strong>O</strong>u<strong>T</strong>lier <strong>A</strong>nalysis
                      <strong>U</strong>sing <strong>R</strong>
                      &#9899;
                    </h4>")
              )
              )
            , style = list('background-color: #ffffcc')) # close wellPanel #ffffe6 #ffffcc
            ,


            ###################
            ## Pretty Plots: ##
            ###################

            wellPanel(
            fluidRow(
              column(2
                     , fluidRow(
                       column(12,
                              div(img(src="scatter.jpg", width=300, height=125), align = "right", height="300px")
                              #,style = "background-color:red;")
                       )
                     )
                     , fluidRow(
                       column(12,
                              div(img(src="manhattan.jpg", width=300, height=125), align = "right", height="300px")
                              #,style = "background-color:red;")
                       )
                     )
              )
              , column(2,
                       div(img(src="circleplot.jpg", width=250, height=250), align = "left", height="600px")
                       #,style = "background-color:blue;")
              ),

              ## repeating for layout's sake ... until we have other pretty figures:
              column(2
                     , fluidRow(
                       column(12,
                              div(img(src="scatter.jpg", width=300, height=125), align = "right", height="300px")
                              #,style = "background-color:red;")
                       )
                     )
                     , fluidRow(
                       column(12,
                              div(img(src="manhattan.jpg", width=300, height=125), align = "right", height="300px")
                              #,style = "background-color:red;")
                       )
                     )
              )
              , column(2,
                       div(img(src="circleplot.jpg", width=250, height=250), align = "left", height="600px")
                       #,style = "background-color:blue;")
              ),

              ## repeating for layout's sake ... until we have other pretty figures:
              column(2
                     , fluidRow(
                       column(12,
                              div(img(src="scatter.jpg", width=300, height=125), align = "right", height="300px")
                              #,style = "background-color:red;")
                       )
                     )
                     , fluidRow(
                       column(12,
                              div(img(src="manhattan.jpg", width=300, height=125), align = "right", height="300px")
                              #,style = "background-color:red;")
                       )
                     )
              )
              , column(2,
                       div(img(src="circleplot.jpg", width=250, height=250), align = "left", height="600px")
                       #,style = "background-color:blue;")
              )

            ),
            style = "background-color:#ccdcff;" #e6eeff#99ccff
            ),


            #             wellPanel(
            #               fluidRow(
            #                 column(6
            #                        , fluidRow(
            #                          column(12,
            #                                 div(img(src="scatter.jpg", width=300, height=125), align = "right", height="300px")
            #                                 #,style = "background-color:red;")
            #                          )
            #                        )
            #                        , fluidRow(
            #                          column(12,
            #                                 div(img(src="manhattan.jpg", width=300, height=125), align = "right", height="300px")
            #                                 #,style = "background-color:red;")
            #                          )
            #                        )
            #                 )
            #                 , column(6,
            #                          div(img(src="circleplot.jpg", width=250, height=250), align = "left", height="600px")
            #                          #,style = "background-color:blue;")
            #                 )
            #
            #               )
            #             ),

            #             wellPanel(
            #               fluidRow(
            #                 column(4,
            #                        div(img(src="scatter.jpg", width=100), align = "center")
            #                        ),
            #                 column(4,
            #                        div(img(src="manhattan.jpg", width=100), align = "center")
            #                 ),
            #                 column(4,
            #                        div(img(src="circleplot.jpg", width=100), align = "center")
            #                 )
            #               )
            #             ),



            ##################
            ## Description: ##
            ##################
            wellPanel(
              fluidRow(
                column(12,
                       HTML(
                         "<h4><strong><i>Welcome to the labyrinth!</i></strong></h4>"
                       ),
                       #HTML("<h6 style='padding: 0px 10px;'></h6>"),
                       p("MINOTAUR is an R package for the detection and visualisation of outliers in multivariate space."),

                       p("The package contains a number of stand-alone functions for outlier detection that can be run in R.
                  Naturally, however, the infamous MINOTAUR is most at home within the labyrinth.
                  This labyrinthine app provides a user-friendly interface through which users can interact with MINOTAUR
                  and explore complex multivariate data with ease."),

                       p("Our package has been designed with genomic data in mind,
                  but it can be applied to multivariate data from any domain.")
                )
              ),
            #),
            #################
            ## Navigation: ##
            #################
            hr(), #########################################################################################
            p(strong("Navigation:")),
            #br(),
            p("To find your way through the labyrinth, use the drop-down menus at the top of the page for navigation."),
            p("Begin by clicking on the", strong("Data"), "tab to
              select an example dataset or input your own data on the", em("Input Data"), "page.
              You can then subset your data on the ", em("Clean-up Data"), "page, if desired,
              but please note that you should ", em("not"), "attempt to remove outliers at this stage."),
            p("Outlier detection will be performed alongside visualisation within the ", strong("Produce Plots"),
              "tab which currently implements ", em("Scatter, Manhattan, "), "and ", em("Circle Plots.")),
            ###################
            ## Useful Links: ##
            ###################
            #wellPanel(
            br(),
            hr(), #########################################################################################
              p(strong("Useful Links:")),

            fluidRow(
              column(2,
                     Reduce(tagAppendChild,Map(
                       function(...)tags$li(tags$a(...,target="_blank"),tabindex="-1"),
                       names(URL_MINOTAUR),href=URL_MINOTAUR),
                       tags$ul(class="nav nav-list",role="menu",`aria-labelledby`="dropdownMenu"))
              ),
              column(1,
                     HTML("<h4 style='padding: 0px 0px;'>
                      &#9899;</h4>")),
              column(2,
                     Reduce(tagAppendChild,Map(
                       function(...)tags$li(tags$a(...,target="_blank"),tabindex="-1"),
                       names(URL_NESCent),href=URL_NESCent),
                       tags$ul(class="nav nav-list",role="menu",`aria-labelledby`="dropdownMenu"))
              ),
              column(1,
                     HTML("<h4 style='padding: 0px 0px;'>
                      &#9899;</h4>")),
              column(2,
                     Reduce(tagAppendChild,Map(
                       function(...)tags$li(tags$a(...,target="_blank"),tabindex="-1"),
                       names(URL_Bug),href=URL_Bug),
                       tags$ul(class="nav nav-list",role="menu",`aria-labelledby`="dropdownMenu"))
              ),
              column(1,
                     HTML("<h4 style='padding: 0px 0px;'>
                      &#9899;</h4>")),
              column(2,
                     Reduce(tagAppendChild,Map(
                       function(...)tags$li(tags$a(...,target="_blank"),tabindex="-1"),
                       names(URL_Contact),href=URL_Contact),
                       tags$ul(class="nav nav-list",role="menu",`aria-labelledby`="dropdownMenu"))
              )
            ) # close fluidRow
            ) # close wellPanel



          ##############################################
          ### MOVE BULK OF CONTENT BELOW TO HELP TAB! ##   ###   ###   ###   ###   ###   ###   ###   ###   ###
          ##############################################


  ) # Close fluidPage
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
                                style = "background-color: #eaf0f4; border-color:#5b95c2; padding:10px"
                                ),

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
                 ), # end Clean-up data page

                 ############################
                 ## CLEANUP DATA PAGE - V2 ##
                 ###########################



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
                              radioButtons("scatter_Checkbox_x", "log(x-axis)", choices = c("log2","log10", "none"),inline = TRUE,selected = "none"),
                              radioButtons("scatter_Checkbox_x_flip", "Flip X", choices = c("Yes", "No"),inline = TRUE,selected = "No"),

                              uiOutput("scatter_ySelection"),
                              radioButtons("scatter_Checkbox_y", "log(y-axis)", choices = c("log2","log10", "none"),inline = TRUE,selected = "none"),
                              radioButtons("scatter_Checkbox_y_flip", "Flip y", choices = c("Yes", "No"),inline = TRUE,selected = "No"),


                              textInput(inputId="scatter_nbins", label="Number of bins", value = 100),

                              uiOutput("scatter_colVarSelection"),
                              textInput(inputId="scatter_cutoff",
                                        label="Percentile cutoff for outliers to overlay (between 0 and 1. If blank: default lower 1% tail.)", value = NULL),
                              radioButtons("scatter_cutoff_tail", "Tail", choices = c("Lower", "Upper"),inline = TRUE,selected = "Lower")#   ,

                              #uiOutput("scatter_colPal")
                            ),

                            # main panel
                            mainPanel(
                              h3('Scatterplot'),
                              p("Warning: this plot is for continuous variables. Do not plot factors."),
                              plotOutput("scatterplot1"),
                              h4('Making the scatterplot'),
                              p("First, choose x and y variables to plot.
                                You can choose whether to log-transform the variables, but note that log-transform of negative numbers is NA and will not be plotted.
                                (Need to add message when try to log-transform negative values.)
                                Next, you can overlay points in the plot according to a third variable of your choice.
                                The value in this box should be between 0 and 1, according to the percentile on the distribution
                                By default, the lower 1% of the y-axis variable will be plotted.
                                For example, choose 'Trait3_p' to see which outliers in Trait3 are also outliers in Trait1.
                              "
                              ),
                              p("To do: (1) Add zoom sliding bars for x-axis and y-axis
                                (2) Add flip options for x and y axis log-axis option
                                (2.5)  Make friendly to new datasets (choose numeric)
                                (3) Can we get the mouse to tell us the name of a point (!)."),
                              h4("Outliers"),
                              p("The table below lists the outliers: the data points below (or above, depending on your selection)
                                the threshold chosen for 'Cutoff for outliers to overlay'."
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
                                radioButtons("logy1Checkbox", "log(Y-axis)", choices = c("log2","log10", "none"),inline = TRUE,selected = "none"),
                                radioButtons("flipY", "Flip Y-axis", choices = c("Yes", "No"),inline = TRUE,selected = "No"),
                                textInput(inputId="linearmht_nbins", label="Number of bins", value = 100),
                                style = "background-color: #eaf0f4; border-color:#5b95c2; padding:10px"),
                              br(),

                              wellPanel(
                                uiOutput('linearMH_p2Selection'),
                                textInput(inputId="linearmhtpcut", label="Cutoff for outliers to overlay", value = 0.01),
                                style = "background-color: #eaf0f4; border-color:#5b95c2; padding:10px",
                                br())
                              #uiOutput('select_linearMH')
                            ),

                            # main panel
                            mainPanel(
                              h3('Manhattan plot'),
                              plotOutput("LinearMHTplot", height="450px",width="800px"),
                              h4('Making the Manhattan plot'),
                              p("First, choose chromosome and coordinate variables for x axis. Next, choose variable for y axis.
                                You can choose whether to log-transform the variables, but note that negative numbers can not be log-transformed.
                                All selected variables should be numeric, not factors.
                                You can overlay points in the plot according to a second variable of your choice.
                                For example, choose 'Trait2_p' to see which outliers in Trait2 are also outliers in Trait1.
                                "
                              ),
                              h4("\nOutliers"),
                              p("The table below lists the outliers: the data points below
                                the threshold chosen for 'Cutoff for outliers to overlay'."
                              ),
                              dataTableOutput("ManhattanDataTable")
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
                              radioButtons("logV1Checkbox", "log (inner circle) ", choices = c("log2","log10","none"),
                                           inline = TRUE,selected = "none"),
                              style = "background-color: #eaf0f4; border-color:#5b95c2; padding:10px"),
                              br(),

                              wellPanel(
                              uiOutput('circle2mh'),
                              radioButtons("logV2Checkbox", "log (outer circle)", choices = c("log2","log10","none"),
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
      ## 'HELP' PAGE ##
      #################

      # (currently just a place-holder showing how to make a summary table)

      navbarMenu("Help",

                 ####################
                 ## APP NAVIGATION ##
                 ####################
                 tabPanel("App Navigation",
                          fluidPage(
                            ## Main Panel data
                            h2('App Navigation:'),

                            ## DATA
                            wellPanel(
                              h3('1. Data'),
                              hr(), #########################################################################################
                              p('MINOTAUR is supplied with example datasets for users to explore the functions and
                                plotting capabilities of the RShiny App.'),
                              #br(),
                              hr(), #########################################################################################
                              h4('Input Data Page'),
                              h5(strong(em('largeData Example')), align = "center"),
                              p('This example dataset provides data for random SNPs across the human genome and their
                                associations to height and Body Mass Index (BMI). Each SNP has location descriptors including
                                a reference chromosome (Chr) and base pair location (BP). They each also have associated
                                p-values (Trait#_P) and effect size measured as a beta-coefficient (Trait#_Beta).'),
                              br(),
                              h5(strong(em('smallData Example')), align = "center"),
                              p(strong('{Need to add details about this data set when we know which one we are using}'), align = "center"),
                              #br(),
                              hr(), #########################################################################################
                              h5(strong(em('Upload Own Data')), align = "center"),
                              p('The user can also upload their own data sets as either .Rdata or Excel files following the example format below:'),
                              div(img(src="example_dataframe.jpg"), align = "center"),
                              p('- rows can be individuals or for genomic data an example would be SNPs', align = "center"),
                              p('- columns are all statstics (e.g. Fst, GWAS, etc.) measurements (e.g. environmental
                                or phenotypic variables), or descriptors (e.g. chromosome, species, etc.)', align = "center"),
               br(),
               #br(),
               hr(), #########################################################################################
               h4('Clean up Data Page'),
               p('This page allows the user to subset their data by either removal of some data columns or
                 by filtering data based on a column.'),
               p('Once the user selects a column to filter by, an additional menu will pop up that gives the user either a
                 sliding bar to subset values of numerical data (left image below) or a box to remove levels of factor data
                 (right image below). A violin plot will appear for continuous numerical data.'),
               div(img(src="numerical_example.jpg"), align = "center", img(src="factor_example.jpg"), align = "center")
              ), # end DATA wellPanel

               #br(),
               #br(),

              ## PLOTS
              wellPanel(
               h3('2. Plots'),
               hr(), #########################################################################################
               h4('Scatterplot'),
               p('This plot allows the user to compare multiple statistics run on their data by plotting two different
                 statistics on the x and y axes and coloring outliers based on a third input.'),
               br(),
               div(img(src="scatter.jpg"), align="center"),
               hr(), #########################################################################################
               h4('Manhattan Plot'),
               p('This plot is used for genomic data to visualize outlier loci relative to their chromosomal location. To do this, you
                 can plot a variable of interest against chromosome position and then color outliers based on
                 for example, the P-value of a given trait.'),
               br(),
               div(img(src="manhattan.jpg"), align="center"),
               hr(), #########################################################################################
               h4('Circle Plot'),
               p('The circle plot is another plot used for visualization of genomic data, which allows the user
                 to plot multiple traits via the inner and outer circles of points against the chromosomal
                 position.'),
               br(),
               div(img(src="circleplot.jpg"), align="center"),
               br(),
               #h5('See Help page for additional information.'),
               br()

                ) # Close wellPanel
                )
                ),

               ###################
               ## SUMMARY TABLE ##
               ###################
               tabPanel('Creating Summary Table',
                          tableOutput("summaryTable")
                   ),

               ##########
               ## FAQs ##
               ##########
               tabPanel("FAQs",

                  h3('Frequently Asked Questions'),
                  h5(strong('Question 1: Why am I getting errors when I upload my data?')),
                  p('Make sure your dataframe matches the required specifications listed on
                     the Welcome Page. Make sure file is either an Excel or .Rdata file.'),
                  h5(strong('Question 2: Why will my circle plot not plot?')),
                  p('If you have a large dataset, be patient. This plot takes time to load for
                     large data.'),
                  h5(strong('Question 3: Why do I get an error when I try to log transform my data?')),
                  p('If the data you are trying to plot has negative values, you will not be able to
                     log transform the data.'),
                  h5(strong('Question 4: Why does the my data not subset when I filter by a variable?')),
                  p('You have to click the Update Data button after choosing any filters. ')
        )
      )
      )))
