
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
  source("server_welcome.R", local = TRUE)

  # input data page
  source("server_input_data.R", local = TRUE)

  # format data page
  source("server_format_data.R", local = TRUE)

  # find outliers page
  source("server_calc_multivariate.R", local = TRUE)

  # compare outliers page
  source("server_compare_outliers.R", local = TRUE)

  # plot 1D histogram/kernel density page
  source("server_plot_histogram.R", local = TRUE)

  # scatter plot page
  source("server_plot_scatterPlot.R", local = TRUE)

  # linear Manhattan plot page
  source("server_plot_Manhattan.R", local = TRUE)

#   # circular Manhattan plot page
#   source("server_plot_circularManhattan.R", local = TRUE)

  # error messages
  source("server_message.R", local = TRUE)

  ## utils
  source("utils.R", local = TRUE)

  #----------------------------------------------------------------

}
