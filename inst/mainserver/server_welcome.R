
##################
## WELCOME PAGE ##  ------------------------------------------------------------------------------------
##################

#####################
## MINOTAUR Banner ##
#####################

# banner panel
output$MinotaurBanner <- renderUI({
  wellPanel(
    fluidRow(
      column(3,
             div(img(src="minotaur.jpg"), align = "left")
      ),
      column(9,
             HTML("<h1 style='padding: 0px 10px;'><big><big><big><strong>MINOTAUR </strong></big></big></big></h1>
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
output$prettyPlots <- renderUI({
  wellPanel(
    fluidRow(
      column(2
             , fluidRow(
               column(12,
                      div(img(src="minotaur_plots.jpg", width=600, height=200), align = "right", height="300px")
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
output$description <- renderUI({
  wellPanel(

    HTML(
      "<h4><strong><i>Welcome to the labyrinth!</i></strong></h4>"
    ),
    p("MINOTAUR is an R package for the detection and visualization of outliers in multivariate space."),

    p("The package contains a number of stand-alone functions for outlier detection that can be run in R.
      Naturally, however, the infamous MINOTAUR is most at home within the labyrinth.
      This labyrinthine app provides a user-friendly interface through which users can interact with MINOTAUR
      and explore complex multivariate data with ease."),

    p("Our package has been designed with genomic data in mind,
      but it can be applied to multivariate data from any domain."),

    hr(),
    p(strong("Navigation:")),
    #br(),
    p("To find your way through the labyrinth, use the tabbed menus on the left side of the page for navigation."),
    p("Begin by clicking on the", strong("Data"), "tab to
      select an example dataset or input your own data on the", em("Input Data"), "page.
      You can then filter your data on the ", em("Format Data"), "page, if desired."),
    p("MINOTAUR implements four multivariate measures - Mahalanobis, harmonic mean distance, nearest neighbord distance, and kernel density deviance - on the ", strong("Outlier Detection"), "page. Users may wish to skip this page if they have already generated multivariate measures using the standalone MINOTAUR functions or functions available from other packages in R. Multivariate measures calculated outside the MINOTAUR GUI can be loaded as part of a dataframe in the", strong("Data"), "tab. Outlier detection is performed alongside visualization within the ", strong("Produce Plots"),
      "tab which currently implements ", em("Histogram, Scatter, "), "and ", em("Manhattan"), "plots."),

    hr(),
    p(strong("More Information:")),
    #br(),
    p("A detailed introduction and tutorial on MINOTAUR can be viewed by issuing the following command in R:"),
    p("vignette('MINOTAUR')"),
    p("MINOTAUR users should also consult the MINOTAUR publication for more detailed information on the package and its application."),
    p("Robert Verity, Caitlin Collins, Daren C. Card, Sara M. Schaal, Liuyang Wang , Katie E. Lotterhos. MINOTAUR: an R package for visualizing and calculating multivariate outliers in genomic datasets. In Review."),
    br(),
    hr(),
    p(strong("Useful Links:")),

    fluidRow(
      column(2,
             Reduce(tagAppendChild,Map(
               function(...)tags$li(tags$a(...,target="_blank"),tabindex="-1"),
               names(URL_MINOTAUR),href=URL_MINOTAUR),
               tags$ul(class="nav nav-list",role="menu",`aria-labelledby`="dropdownMenu"))
      ),
      column(1,
             HTML("<h4 style='padding: 0px 0px;'>
                  &#9899;</h4>")),
      column(2,
             Reduce(tagAppendChild,Map(
               function(...)tags$li(tags$a(...,target="_blank"),tabindex="-1"),
               names(URL_NESCent),href=URL_NESCent),
               tags$ul(class="nav nav-list",role="menu",`aria-labelledby`="dropdownMenu"))
      ),
      column(1,
             HTML("<h4 style='padding: 0px 0px;'>
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
