
##########
## ui.R ##
##########

# This is the user interface.
# This function is the pretty face of our shiny app.

# TODO
# load data table can go off edge if using wrong delimiter
# load examples
# fix headers and delimiters
# subset page currently subsetting multiple times. Look into proper solution for this (connectors?)

# list of packages definitely used in new version
# shiny
# shinydashboard
# data.table - fast reading in of data
# shinyjs - options for deactivating shiny inputs

# devtools::install_github("Bioconductor-mirror/OmicCircos", ref="b772950")
# devtools::install_github("rstudio/DT", ref="24d71f2")
# ---------------------------------------------------------------------------------------------------------------

ui <- dashboardPage(title= "MINOTAUR",

                    #########################
                    ## TITLE AND SIDE MENU ##  ------------------------------------------------------------------------------------
                    #########################
                    dashboardHeader(title='MINOTAUR', dropdownMenuOutput('messageMenu')),

                    dashboardSidebar(
                      sidebarMenu(

                        # Welcome menu
                        menuItem("Welcome", tabName="welcome", icon=icon("home")),

                        # Data menu
                        menuItem("Data", tabName="data", icon=icon("bars"),
                                 menuSubItem("Input Data", tabName="data_input"),
                                 menuSubItem("Format Data", tabName="data_format")
                        ),

                        # Outlier Detection menu
                        menuItem("Multivariate Measures", tabName="multivariate", icon=icon("calculator"),
                                 menuSubItem("Calculate", tabName="calc_multi")
                                 # menuSubItem("Compare Outliers", tabName="outlier_compare")
                        ),

                        # Produce Plots menu
                        menuItem("Produce Plots", tabName="plots", icon=icon("area-chart"),
                                 menuSubItem("1D Histogram", tabName="plots_histogram_1D"),
                                 menuSubItem("2D Scatterplot", tabName="plots_2D_scatterplot"),
                                 menuSubItem("Linear Manhattan Plot", tabName="plots_linear_Manhattan")
#                                 menuSubItem("Circular Manhattan Plot", tabName="plots_circular_Manhattan")
                        )

                        # Help menu
#                         menuItem("Help", tabName="help", icon=icon("question"))

                      ) # end of sidebarMenu
                    ), # end of dashboardSidebar


                    ################
                    ## MAIN PAGES ##  ------------------------------------------------------------------------------------
                    ################

                    dashboardBody(

                      tabItems(

                        #############
                        ## WELCOME ##
                        #############
                        
                        #### Welcome page
                        tabItem(tabName="welcome",

                                # MINOTAUR banner
                                uiOutput('MinotaurBanner'),

                                # example plots
                                uiOutput('prettyPlots'),

                                # description
                                uiOutput('description')

                        ),# end of Welcome page tabItem

                        ##########
                        ## DATA ##
                        ##########
                        
                        #### Input Data page
                        tabItem(tabName="data_input",
                                fluidRow(
                                  column(4,
                                         # Load Data box
                                         uiOutput("headerBox_loadData"),
                                         uiOutput('tabBox_loadData')
                                  ),
                                  column(8,
                                         # Data Summary box (multiple parts)
                                         uiOutput('box_dataName'),
                                         uiOutput('valueBox_rows'),
                                         uiOutput('valueBox_cols'),
                                         uiOutput('tabBox_rawDataSummary')
                                  )
                                )
                        ), # end of Input Data page

                        #### Format Data page
                        tabItem(tabName="data_format",
                                fluidRow(
                                  column(4,
                                         # Format Data box
                                         uiOutput('box_formatData')
                                  ),
                                  column(8,
                                         # Plot Genomic box
                                         uiOutput('tabBox_plotGenomic')
                                  )
                                ),
                                fluidRow(
                                  column(4,
                                         # Subset Data box
                                         uiOutput('box_subsetData')

                                         # summary of missing data removed
                                         #uiOutput('valueBox_missingDataRemoved')
                                  ),
                                  column(8,
                                         # Final Data box
                                         uiOutput('box_finalData')
                                  )
                                )
                        ), # end of Format Data page

                        ###########################
                        ## MULTIVARIATE MEASURES ##
                        ###########################
                        
                        #### Calculate page
                        tabItem(tabName="calc_multi",
                                fluidRow(
                                  column(7,
                                         # controls for producing compound measures
                                         #uiOutput('headerBox_produce_compound'),
                                         uiOutput('tabBox_produce_compound')
                                  ),
                                  column(5,
                                         # density plot of compound measure
                                         uiOutput('box_histogram_compound')
                                  )
                                )
                         ), # end of Calculate page

                        ###################
                        ## PRODUCE PLOTS ##
                        ###################

                        #### Plot 1D Histogram/Density page
                        tabItem(tabName="plots_histogram_1D",
                                fluidRow(
                                  column(12,
                                         uiOutput("box_hist_1D")
                                  ),
                                  column(4,
                                         uiOutput("box_hist_1D_button")
                                  )
                                )
                        ), # end of 1D Histogram/Density page

                        #### Scatter Plot page
                        tabItem(tabName="plots_2D_scatterplot",
                                fluidRow(
                                  column(12,
                                         uiOutput("box_scatterPlot")
                                  ),
                                  column(4,
                                         uiOutput("box_scatterPlot_button")
                                  )
                                )

                        ), # end of Scatter lot page

                        #### Linear Manhattan Plot page
                        tabItem(tabName="plots_linear_Manhattan",
                                fluidRow(
                                  column(12,
                                         uiOutput("box_linearManhattan")
                                  ),
                                  column(4,
                                         uiOutput("box_linearManhattan_button")
                                  )
                                )
                        ) # end of Linear Manhattan

#                         #### Circular Manhattan Plot page
#                         tabItem(tabName="plots_circular_Manhattan",
#                                 fluidRow(
#                                   column(12,
#                                          uiOutput("box_circularManhattan")
#                                   ),
#                                   column(4,
#                                          uiOutput("box_circularManhattan_button")
#                                   )
#                                 )
#                         ) # end of Circular Manhattan Plot


                      ) # end of tabItems
                    ) # end of dashboardBody
) # end of dashboardPage (end of ui)
