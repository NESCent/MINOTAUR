
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

# ---------------------------------------------------------------------------------------------------------------

ui <- dashboardPage(

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
               menuSubItem("Linear Manhattan Plot", tabName="plots_linear_Manhattan"),
               menuSubItem("1D Histogram/Density", tabName="plots_1D_histogram"),
               menuSubItem("2D Scatterplot", tabName="plots_2D_scatterplot"),
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
                 uiOutput('box_loadData')
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

      #### Linear Manhattan Plot page
      tabItem(tabName="plots_linear_Manhattan",
              fluidRow(
                column(4,
                       # controls for linear Manhattan plots
                       uiOutput('box_linearManhattan_controls')
                ),
                column(8,
                       # navigation panel
                       uiOutput('box_linearManhattan_navigation'),

                       # plot linear Manhattan
                       uiOutput('box_plot_linearManhattan')
                )
              )
      ), # end of Linear Manhattan Plot page tabItem

      #### Plot 1D Histogram/Density page
      tabItem(tabName="plots_1D_histogram",
              fluidRow(
                column(4,
                       #
                       uiOutput('box_1D_hist_controls')
                ),
                column(8,
                       #
                       uiOutput('box_1D_hist_plot')
                )
              )
      ) # end of 1D Histogram/Density page tabItem

    ) # end of tabItems
  ) # end of dashboardBody
) # end of dashboardPage (end of ui)
