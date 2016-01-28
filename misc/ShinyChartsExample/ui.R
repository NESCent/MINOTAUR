
# ui.R (one of three scripts required to run any Rshiny app)

# Author: Bob Verity
# Date: 02/04/2014

# Purpose:
# Defines the user interface, including any controls that the user interacts with. Interactive objects from within this script pass values to the system script that can be used as ordinary variables.

#### ------------------------------------------------------------------

# The pageWithSidebar option is a convenient design template. You can also create more customised layouts by digging a little deeper.
shinyUI(pageWithSidebar(
  headerPanel("Shiny Charts Example"),
  
  # This is a panel on the left hand side where we can put our dials
  sidebarPanel(
  
  # Heading and some intro text
  h2("Random Bubbles!"),
    p("The package rCharts in combination with rShiny can be used to produce nice interactive animated graphs."),
   p("This is a simple example of a slider and a couple of drop-down menus. Press the \"submit\" button to implement your choices."),
  
  # Put a slider and a couple of dropdown menus in a wellPanel
  wellPanel(
  
	# create slider
	sliderInput("interactionStrength", "Interaction strength:", min=0, max=1, value=0, step=0.01),
  
     # insert line break
     br(),
  
	# create dropdown menus
	selectInput(inputId = "xSelection",
      label = "Choose x-axis data:",
      choices = c("Normal", "Gamma", "Beta"),
      selected = "Normal"),
      
	selectInput(inputId = "ySelection",
      label = "Choose y-axis data:",
      choices = c("Normal", "Gamma", "Beta"),
      selected = "Gamma"),
     
     # couple of line breaks
     br(),
     br(),
    
    # Using a submitButton means interactive values (and plots relying on them) are only updated when this button is pressed. This can be useful if plots are computationally expensive to produce or render.
    submitButton("Update plots")
      
  	)
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
  
))
