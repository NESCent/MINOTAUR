
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

#devtools::install_github('rstudio/DT')


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
      menuItem("Outlier Detection", tabName="outlier", icon=icon("search"),
               menuSubItem("Find Outliers", tabName="outlier_find"),
               menuSubItem("Compare Outliers", tabName="outlier_compare")
      ),

      # Produce Plots menu
      menuItem("Produce Plots", tabName="plots", icon=icon("area-chart"),
               menuSubItem("1D Histogram", tabName="plots_histogram_1D"),
               menuSubItem("2D Scatterplot", tabName="plots_2D_scatterplot"),
               menuSubItem("Linear Manhattan Plot", tabName="plots_linear_Manhattan"),
               menuSubItem("Circular Manhattan Plot", tabName="plots_circular_Manhattan")
      ),

      # Help menu
      menuItem("Help", tabName="help", icon=icon("question"))

    ) # end of sidebarMenu
  ), # end of dashboardSidebar


  ################
  ## MAIN PAGES ##  ------------------------------------------------------------------------------------
  ################

  dashboardBody(
    tabItems(

      #### Welcome page
      tabItem(tabName="welcome",

        # MINOTAUR banner
        uiOutput('MinotaurBanner'),

        # example plots
        uiOutput('prettyPlots'),

        # description
        uiOutput('description')

      ),# end of Welcome page tabItem

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
      ), # end of Input Data page tabItem

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
      ), # end of Format Data page tabItem

      #### Find Outliers page
      tabItem(tabName="outlier_find",
              fluidRow(
                column(7,
                       # controls for producing compound measures
                       uiOutput('tabBox_produce_compound')
                ),
                column(5,
                       # density plot of compound measure
                       uiOutput('box_density_compound')
                )
              ),
              fluidRow(
                column(4,
                       #
                       uiOutput('box_choose_threshold')
                ),
                column(8,
                       #
                       uiOutput('box_list_outliers')
                )
              )
      ), # end of Find Outliers page tabItem

      #### Compare Outliers page
      tabItem(tabName="outlier_compare",
              fluidRow(
                column(4,
                       #
                       uiOutput('box_compare1')
                ),
                column(8,
                       #
                       uiOutput('box_compare2')
                )
              )
      ), # end of Compare Outliers page tabItem

      ###################
      ## PRODUCE PLOTS ##
      ###################

      #### Plot 1D Histogram/Density page
      tabItem(tabName="plots_histogram_1D",
              fluidRow(
                column(4,
                       # controls for 1D hist
                       uiOutput("box_hist_1D_controls"),
                       uiOutput("box_hist_1D_button")
                ),
                column(8,
                       # plot 1D hist
                       uiOutput("box_hist_1D_plot"),

                       # navigation panel
                       uiOutput("box_hist_1D_controls_aes")
                )
              )
      ), # end of 1D Histogram/Density page tabItem

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
                  #                 column(4,
                  #                        # controls for linear Manhattan plots
                  #                        uiOutput("box_scatterPlot_controls"),
                  #
                  #                        uiOutput("box_scatterPlot_button")
                  #                        #textOutput("buttonTest_scatterPlot")
                  #                 ),
                  #                 column(8,
                  #                        # plot linear Manhattan
                  #                        uiOutput("box_scatterPlot_plot"),
                  #
                  #                        # navigation panel
                  #                        uiOutput("box_scatterPlot_controls_aes")
                  #                 )

      ), # end of Scatter lot page tabItem

      #### Linear Manhattan Plot page
      tabItem(tabName="plots_linear_Manhattan",
              fluidRow(
                column(4,
                       # controls for linear Manhattan plots
                       uiOutput("box_linearManhattan_controls"),

                       uiOutput("box_linearManhattan_button")
                       #textOutput("buttonTest_linearManhattan")
                ),
                column(8,
                       # plot linear Manhattan
                       uiOutput("box_plot_linearManhattan"),

                       # navigation panel
                       uiOutput("box_linearManhattan_controls_aes")
                )
              )
      ), # end of Linear Manhattan Plot page tabItem

      #### Circular Manhattan Plot page
      tabItem(tabName="plots_circular_Manhattan",
      fluidRow(
        column(4,
               # controls for circular Manhattan plots
               uiOutput("box_circularManhattan_controls"),

               uiOutput("box_circularManhattan_button")
               #textOutput("buttonTest_circularManhattan")
        ),
        column(8,
               # plot circular Manhattan
               uiOutput("box_plot_circularManhattan"),

               # navigation panel
               uiOutput("box_circularManhattan_controls_aes")
        )
      )
    ) # end of Circular Manhattan Plot page tabItem


    ) # end of tabItems
  ) # end of dashboardBody
) # end of dashboardPage (end of ui)
