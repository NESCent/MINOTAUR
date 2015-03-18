
# ui.R (one of three scripts required to run any Rshiny app)

# Author: Bob Verity
# Date: 17/03/2015

# Purpose:
# Defines the user interface, including any controls that the user interacts with. Interactive objects from within this script pass values to the system script that can be used as ordinary variables.

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
    )
  
))
