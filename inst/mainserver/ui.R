
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
#' @importFrom shinydashboard dashboardPage
#' @importFrom shinydashboard dashboardHeader
#' @importFrom shinydashboard dashboardSidebar
#' @importFrom shinydashboard sidebarMenu
#' @importFrom shinydashboard menuItem
#' @importFrom shinydashboard menuSubItem
#' @importFrom shinydashboard tabItems
#' @importFrom shinydashboard tabItem
#' @importFrom shinydashboard dropdownMenuOutput
#' @importFrom shiny fluidRow
#' @importFrom shiny uiOutput

#' @keywords internal
# ---------------------------------------------------------------------------------------------------------------
ui <- shinydashboard::dashboardPage(
  title= "MINOTAUR",

  #########################
  ## TITLE AND SIDE MENU ##  ------------------------------------------------------------------------------------
  #########################
  shinydashboard::dashboardHeader(
    title='MINOTAUR', shinydashboard::dropdownMenuOutput('messageMenu')),

  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(

      # Welcome menu
      shinydashboard::menuItem("Welcome", tabName="welcome", icon=icon("home")),

      # Data menu
      shinydashboard::menuItem(
        "Data", tabName="data", icon=icon("bars"),
        shinydashboard::menuSubItem("Input Data", tabName="data_input"),
        shinydashboard::menuSubItem("Format Data", tabName="data_format")
      ),

      # Outlier Detection menu
      shinydashboard::menuItem(
        "Multivariate Measures", tabName="multivariate", icon=icon("calculator"),
        shinydashboard::menuSubItem("Calculate", tabName="calc_multi")
        # shinydashboard::menuSubItem("Compare Outliers", tabName="outlier_compare")
      ),

      # Produce Plots menu
      shinydashboard::menuItem(
        "Produce Plots", tabName="plots", icon=icon("area-chart"),
        shinydashboard::menuSubItem("1D Histogram", tabName="plots_histogram_1D"),
        shinydashboard::menuSubItem("2D Scatterplot", tabName="plots_2D_scatterplot"),
        shinydashboard::menuSubItem("Linear Manhattan Plot", tabName="plots_linear_Manhattan")
        #                                 shinydashboard::menuSubItem("Circular Manhattan Plot", tabName="plots_circular_Manhattan")
      )

      # Help menu
      # shinydashboard::menuItem("Help", tabName="help", icon=icon("question"))

    ) # end of sidebarMenu
  ), # end of dashboardSidebar


  ################
  ## MAIN PAGES ##  ------------------------------------------------------------------------------------
  ################

  shinydashboard:: dashboardBody(

    shinydashboard::tabItems(

      #############
      ## WELCOME ##
      #############

      #### Welcome page
      shinydashboard::tabItem(
        tabName="welcome",

        # MINOTAUR banner
        shiny::uiOutput('MinotaurBanner'),

        # example plots
        shiny::uiOutput('prettyPlots'),

        # description
        shiny::uiOutput('description')

      ),# end of Welcome page tabItem

      ##########
      ## DATA ##
      ##########

      #### Input Data page
      shinydashboard::tabItem(
        tabName="data_input",
        shiny::fluidRow(
          column(4,
                 # Load Data box
                 shiny::uiOutput("headerBox_loadData"),
                 shiny::uiOutput('tabBox_loadData')
          ),
          column(8,
                 # Data Summary box (multiple parts)
                 shiny::uiOutput('box_dataName'),
                 shiny::uiOutput('valueBox_rows'),
                 shiny::uiOutput('valueBox_cols'),
                 shiny::uiOutput('tabBox_rawDataSummary')
          )
        )
      ), # end of Input Data page

      #### Format Data page
      shinydashboard::tabItem(
        tabName="data_format",
        shiny::fluidRow(
          column(4,
                 # Format Data box
                 shiny::uiOutput('box_formatData')
          ),
          column(8,
                 # Plot Genomic box
                 shiny::uiOutput('box_plotBreakdown')
          )
        ),
        shiny::fluidRow(
          column(4,
                 # Subset Data box
                 shiny::uiOutput('box_subsetData'),

                 # summary of missing data removed
                 shiny::uiOutput('valueBox_missingDataRemoved')
          ),
          column(8,
                 # Final Data box
                 shiny::uiOutput('box_finalData')
          )
        )
      ), # end of Format Data page

      ###########################
      ## MULTIVARIATE MEASURES ##
      ###########################

      #### Calculate page
      shinydashboard::tabItem(
        tabName="calc_multi",
        shiny::fluidRow(
          column(7,
                 # controls for producing compound measures
                 #shiny::uiOutput('headerBox_produce_compound'),
                 shiny::uiOutput('tabBox_produce_compound')
          ),
          column(5,
                 # density plot of compound measure
                 shiny::uiOutput('box_histogram_compound')
          )
        )
      ), # end of Calculate page

      ###################
      ## PRODUCE PLOTS ##
      ###################

      #### Plot 1D Histogram/Density page
      shinydashboard::tabItem(
        tabName="plots_histogram_1D",
        shiny::fluidRow(
          column(12,
                 shiny::uiOutput("box_hist_1D")
          ),
          column(4,
                 shiny::uiOutput("box_hist_1D_button")
          )
        )
      ), # end of 1D Histogram/Density page

      #### Scatter Plot page
      shinydashboard::tabItem(
        tabName="plots_2D_scatterplot",
        shiny::fluidRow(
          column(12,
                 shiny::uiOutput("box_scatterPlot")
          ),
          column(4,
                 shiny::uiOutput("box_scatterPlot_button")
          )
        )

      ), # end of Scatter lot page

      #### Linear Manhattan Plot page
      shinydashboard::tabItem(
        tabName="plots_linear_Manhattan",
        shiny::fluidRow(
          column(12,
                 shiny::uiOutput("box_linearManhattan")
          ),
          column(4,
                 shiny::uiOutput("box_linearManhattan_button")
          )
        )
      ) # end of Linear Manhattan

      # #### Circular Manhattan Plot page
      # shinydashboard::tabItem(
      #   tabName="plots_circular_Manhattan",
      #   shiny::fluidRow(
      #     column(12,
      #            shiny::uiOutput("box_circularManhattan")
      #     ),
      #     column(4,
      #            shiny::uiOutput("box_circularManhattan_button")
      #     )
      #   )
      # ) # end of Circular Manhattan Plot


    ) # end of tabItems
  ) # end of dashboardBody
) # end of dashboardPage (end of ui)
