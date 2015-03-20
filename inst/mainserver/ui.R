
##########
## ui.R ##
##########

# This is the user interface.
# This function is the pretty face of our shiny app.


# Load packages
require("shiny")
require("rCharts")
require("rHighcharts")
require("stats4")
require("adegenet")


#### ------------------------------------------------------------------


# The pageWithSidebar option is a convenient design template. You can also create more customised layouts by digging a little deeper.
shinyUI(navbarPage("MANIPULATE",
  tabPanel("Welcome",
    h2("hello world")
  ),

  tabPanel("Cleanup data",
    dataTableOutput("table2")
  ),
  tabPanel("Produce plots",
    fluidRow(
      column(4,
        h2('booboo'),
        p('here is some text')
      ),
      column(6,
        plotOutput("plot1")
      )
    )
  ),
  navbarMenu("More",
      tabPanel("(sub-menu A)"),
      tabPanel("(sub-menu B)")
    ),

  
  # The main panel is where we will output our plots (from server.R)
  mainPanel(
  
	# bubble plot
  showOutput("bubbleChart1", "Highcharts"),
    
	# histogram
	plotOutput("plot1",height="300px",width="300px"),
	
	# summary table
	tableOutput("table1")
	
  )
  
    )
  
)
