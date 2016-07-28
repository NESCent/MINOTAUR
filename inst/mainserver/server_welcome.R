
##################
## WELCOME PAGE ##  ------------------------------------------------------------------------------------
##################

#####################
## MINOTAUR Banner ##
#####################

#' @importFrom shiny p
#' @importFrom shiny h3
#' @importFrom shiny br
#' @importFrom shiny strong
#' @importFrom shiny wellPanel
#' @importFrom shiny fluidRow
#' @importFrom shiny div
#' @importFrom shiny HTML
#' @importFrom shiny fluidRow
#' @importFrom shiny em

# banner panel
output$MinotaurBanner <- shiny::renderUI({
  shiny::wellPanel(
    shiny::fluidRow(
      column(3,
             shiny::div(img(src="minotaur.jpg"), align = "left")
      ),
      column(9,
             shiny::HTML("<h1 style='padding: 0px 10px;'><big><big><big><strong>MINOTAUR </strong></big></big></big></h1>
                  <h4 style='padding: 0px 10px;'>
                  &#9899;
                  <strong>M</strong>ult<strong>I</strong>variate
                  visualisatio<strong>N</strong>
                  and <strong>O</strong>u<strong>T</strong>lier <strong>A</strong>nalysis
                  <strong>U</strong>sing <strong>R</strong>
                  &#9899;
                  </h4>")
             )
             ),

    # style = list('background-color: #ffffcc')
    style = list('background-color: #FFECB3') # pale amber
             )
})

##################
## Pretty Plots ##
##################

# example plots
output$prettyPlots <- shiny::renderUI({
  shiny::wellPanel(
    shiny::fluidRow(
      column(2
             ,
             shiny::fluidRow(
               column(12,
                      shiny::div(img(src="minotaur_plots.jpg", width=600, height=200), align = "right", height="300px")
               )
             )
      )
#       column(2
#              , fluidRow(
#                column(12,
#                       div(img(src="manhattan.jpg", width=300, height=200), align = "left", height="300px", offset=2)
#                )
#              )
#       )
    )
  )
})

#################
## Description ##
#################

# example plots
output$description <- shiny::renderUI({
  shiny::wellPanel(

    shiny::HTML(
      "<h4><strong><i>Welcome to the labyrinth!</i></strong></h4>"
    ),
    shiny::p("MINOTAUR is an R package for the detection and visualization of outliers in multivariate space."),

    shiny::p("The package contains a number of stand-alone functions for outlier detection that can be run in R.
      Naturally, however, the infamous MINOTAUR is most at home within the labyrinth.
      This labyrinthine app provides a user-friendly interface through which users can interact with MINOTAUR
      and explore complex multivariate data with ease."),

    shiny::p("Our package has been designed with genomic data in mind,
      but it can be applied to multivariate data from any domain."),

    shiny::hr(),
    shiny::p(shiny::strong("Navigation:")),
    #br(),
    shiny::p("To find your way through the labyrinth, use the tabbed menus on the left side of the page for navigation."),
    shiny::p("Begin by clicking on the", shiny::strong("Data"), "tab to
      select an example dataset or input your own data on the", shiny::em("Input Data"), "page.
      You can then filter your data on the ", shiny::em("Format Data"), "page, if desired."),
    shiny::p("MINOTAUR implements four multivariate measures - Mahalanobis, harmonic mean distance, nearest neighbor distance, and kernel density deviance - on the ", shiny::strong("Multivariate Measures"), "page. Users may wish to skip this page if they have already generated multivariate measures using the standalone MINOTAUR functions or functions available from other packages in R. Multivariate measures calculated outside the MINOTAUR GUI can be loaded as part of a dataframe in the", shiny::strong("Data"), "tab. Outlier detection is performed alongside visualization within the ", shiny::strong("Produce Plots"),
      "tab which currently implements ", shiny::em("Histogram, Scatter, "), "and ", shiny::em("Manhattan"), "plots."),

    shiny::hr(),
    shiny::p(shiny::strong("More Information:")),
    #br(),
    shiny::p("A detailed introduction and tutorial on MINOTAUR can be viewed by issuing the following command in R:"),
    shiny::p("vignette('MINOTAUR')"),
    shiny::p("MINOTAUR users should also consult the MINOTAUR publication for more detailed information on the package and its application."),
    shiny::p("Robert Verity, Caitlin Collins, Daren C. Card, Sara M. Schaal, Liuyang Wang , Katie E. Lotterhos. MINOTAUR: an R package for visualizing and calculating multivariate outliers in genomic datasets. In Review."),
    shiny::br(),
    shiny::hr(),
    shiny::p(shiny::strong("Useful Links:")),

    shiny::fluidRow(
      column(2,
             Reduce(tagAppendChild,Map(
               function(...)tags$li(tags$a(...,target="_blank"),tabindex="-1"),
               names(URL_MINOTAUR),href=URL_MINOTAUR),
               tags$ul(class="nav nav-list",role="menu",`aria-labelledby`="dropdownMenu"))
      ),
      column(1,
             shiny::HTML("<h4 style='padding: 0px 0px;'>
                  &#9899;</h4>")),
      column(2,
             Reduce(tagAppendChild,Map(
               function(...)tags$li(tags$a(...,target="_blank"),tabindex="-1"),
               names(URL_NESCent),href=URL_NESCent),
               tags$ul(class="nav nav-list",role="menu",`aria-labelledby`="dropdownMenu"))
      ),
      column(1,
             shiny::HTML("<h4 style='padding: 0px 0px;'>
                  &#9899;</h4>")),
      column(2,
             Reduce(tagAppendChild,Map(
               function(...)tags$li(tags$a(...,target="_blank"),tabindex="-1"),
               names(URL_Bug),href=URL_Bug),
               tags$ul(class="nav nav-list",role="menu",`aria-labelledby`="dropdownMenu"))
      )
 #     column(1,
 #            HTML("<h4 style='padding: 0px 0px;'>
 #                 &#9899;</h4>")),
 #     column(2,
 #            Reduce(tagAppendChild,Map(
 #              function(...)tags$li(tags$a(...,target="_blank"),tabindex="-1"),
 #              names(URL_Contact),href=URL_Contact),
 #              tags$ul(class="nav nav-list",role="menu",`aria-labelledby`="dropdownMenu"))
 #     )
             ) # close fluidRow
      ) # close wellPanel
})
