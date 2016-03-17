
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
# DT - used in raw data table summary
# shinyjs - options for deactivating shiny inputs
# ggplot2

# loading packages here. Do these need to be moved?
require("shinydashboard")
require("ggvis")

# ---------------------------------------------------------------------------------------------------------------

ui <- dashboardPage(

  #########################
  ## TITLE AND SIDE MENU ##  ------------------------------------------------------------------------------------
  #########################
  dashboardHeader(title='MINOTAUR'),

  dashboardSidebar(
    sidebarMenu(

      # Welcome menu
      menuItem("Welcome", tabName="welcome", icon=icon("home")),

      # Data menu
      menuItem("Data", tabName="data", icon=icon("bars"),
               menuSubItem("Input Data", tabName="data_input"),
               menuSubItem("Cleanup Data", tabName="data_cleanup")
      ),

      # Outlier Detection menu
      menuItem("Outlier Detection", tabName="outlier", icon=icon("search"),
               menuSubItem("Find Outliers", tabName="outlier_find"),
               menuSubItem("List Outliers", tabName="outlier_list")
      ),

      # Produce Plots menu
      menuItem("Produce Plots", tabName="plots", icon=icon("area-chart"),
               menuSubItem("1D Histogram", tabName="plots_1D_histogram"),
               menuSubItem("1D Kernel Density", tabName="plots_1D_density"),
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

      ),

      #### Input Data page
      tabItem(tabName="data_input",
        fluidRow(

          # Load Data box
          uiOutput('box_loadData'),

          # Data Summary box (multiple parts)
          uiOutput('box_dataName'),
          uiOutput('valueBox_rows'),
          uiOutput('valueBox_cols'),
          uiOutput('box_rawDataSummary')

        )
      ),

      #### Cleanup Data page
      tabItem(tabName="data_cleanup",
              fluidRow(
                column(4,
                  # Subset Data box
                  uiOutput('box_subsetData'),

                  #
                  uiOutput('valueBox_missingDataRemoved')
                ),
                column(8,

                  # Final Data box
                  uiOutput('box_cleanupData')
                )
              )
      ),

      #### Find Outliers page
      tabItem(tabName="outlier_find",
              fluidRow(

                column(4,

                       # global controls for univariate plots
                       uiOutput('box_global_univariate'),

                       # individual controls for univariate plots
                       uiOutput('box_controls_univariate')

                ),
                column(8,

                       # navigation panel
                       uiOutput('box_navigation'),

                       # univariate plots
                       uiOutput('box_plot_univariate')
                )

              ),
              fluidRow(

                # controls for distance-based plots
                uiOutput('box_control_distanceBased'),

                # distance-based plots
                uiOutput('box_plot_distanceBased')

              ),
              fluidRow(

                # controls for density-based plots
                uiOutput('box_control_densityBased'),

                # density-based plots
                uiOutput('box_plot_densityBased')

              )
      )

    ) # end of tabItems
  ) # end of dashboardBody
) # end of dashboardPage (end of ui)
