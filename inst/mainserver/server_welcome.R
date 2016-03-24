
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
             ), style = list('background-color: #ffffcc')
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
                      div(img(src="scatter.jpg", width=300, height=125), align = "right", height="300px")
                      #,style = "background-color:red;")
               )
             )
             , fluidRow(
               column(12,
                      div(img(src="manhattan.jpg", width=300, height=125), align = "right", height="300px")
                      #,style = "background-color:red;")
               )
             )
      )
      , column(2,
               div(img(src="circleplot.jpg", width=250, height=250), align = "left", height="600px")
               #,style = "background-color:blue;")
      )
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
    p("MINOTAUR is an R package for the detection and visualisation of outliers in multivariate space."),

    p("The package contains a number of stand-alone functions for outlier detection that can be run in R.
      Naturally, however, the infamous MINOTAUR is most at home within the labyrinth.
      This labyrinthine app provides a user-friendly interface through which users can interact with MINOTAUR
      and explore complex multivariate data with ease."),

    p("Our package has been designed with genomic data in mind,
      but it can be applied to multivariate data from any domain."),

    hr(),
    p(strong("Navigation:")),
    #br(),
    p("To find your way through the labyrinth, use the drop-down menus at the top of the page for navigation."),
    p("Begin by clicking on the", strong("Data"), "tab to
      select an example dataset or input your own data on the", em("Input Data"), "page.
      You can then subset your data on the ", em("Clean-up Data"), "page, if desired,
      but please note that you should ", em("not"), "attempt to remove outliers at this stage."),
    p("Outlier detection will be performed alongside visualisation within the ", strong("Produce Plots"),
      "tab which currently implements ", em("Scatter, Manhattan, "), "and ", em("Circle Plots.")),

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
      ),
      column(1,
             HTML("<h4 style='padding: 0px 0px;'>
                  &#9899;</h4>")),
      column(2,
             Reduce(tagAppendChild,Map(
               function(...)tags$li(tags$a(...,target="_blank"),tabindex="-1"),
               names(URL_Contact),href=URL_Contact),
               tags$ul(class="nav nav-list",role="menu",`aria-labelledby`="dropdownMenu"))
      )
             ) # close fluidRow
      ) # close wellPanel
})
