
## Adding useful links to our App (Welcome Page only?) ##
URL_list <- list("MINOTAUR" = "https://github.com/NESCent/MINOTAUR",
                 "NESCent" = "http://nescent.org/",
                 "Report a Bug" = "https://github.com/NESCent/MINOTAUR/issues/new",
                 "Contact the Developers" =
                   "mailto:caitlin.collins12@@imperial.ac.uk,
                 r.verity@imperial.ac.uk,
                 k.lotterhos@neu.edu,
                 dcard@uta.edu,
                 schaal.s@husky.neu.edu,
                 wallacewly@gmail.com")

URL_MINOTAUR <- list('MINOTAUR' = "https://github.com/NESCent/MINOTAUR")
URL_NESCent <- list("NESCent" = "http://nescent.org/")
URL_Bug <- list("Report Bugs" = "https://github.com/NESCent/MINOTAUR/issues/new")
URL_Contact <- list("Contact Us" =
                      "mailto:caitiecollins@gmail.com,
                    r.verity@imperial.ac.uk,
                    k.lotterhos@neu.edu,
                    dcard@uta.edu,
                    schaal.s@husky.neu.edu,
                    wallacewly@gmail.com")

# ---------------------------------------------------------------------------------------------------------------

server <- function(input, output) {

  # welcome page
  source("server_welcome.R", local=T)

  # input data page
  source("server_input_data.R", local=T)

  # format data page
  source("server_format_data.R", local=T)

  # find outliers page
  source("server_find_outliers.R", local=T)

  # compare outliers page
  source("server_compare_outliers.R", local=T)

  # plot 1D histogram/kernel density page
  source("server_plot_histogram.R", local=T)

  # scatter plot page
  source("server_plot_scatterPlot.R", local=T)

  # linear Manhattan plot page
  source("server_plot_Manhattan.R", local=T)

  # circular Manhattan plot page
  source("server_plot_circularManhattan.R", local=T)

  # error messages
  source("server_message.R", local=T)

  ## utils
  source("utils.R", local=T)

  #----------------------------------------------------------------
  # CRAP THAT I'M KEEPING FOR NOW JUST IN CASE IT BECOMES USEFUL
  # html load data button (bit nicer than shiny default)
  output$loadDataButton <- renderUI({
    fluidRow(
      column(12,
             div(style="display:inline-block",
                 tags$button(id='loadDataButton', type="button",
                             class="btn action-button btn-primary",
                             style='font-size:15px; text-align:center',
                             HTML('<i class="icon-star"></i>Load Data')),
                 HTML(paste('<input type="text" value="',rv$data_name,'" readonly="readonly">',sep=''))
             ),
             align='center')
    )
  })
  observeEvent(input$loadDataButton, {
    print("foobar")
  })

  #----------------------------------------------------------------

}
