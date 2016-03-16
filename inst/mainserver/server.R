
## Adding useful links to our App (Welcome Page only?) ##
URL_list <- list("MINOTAUR" = "https://github.com/NESCent/MINOTAUR",
                 "NESCent" = "http://nescent.org/",
                 "Report a Bug" = "https://github.com/NESCent/MINOTAUR/issues/new",
                 "Contact the Developers" =
                   "mailto:caitiecollins@gmail.com,
                 r.verity@imperial.ac.uk,
                 k.lotterhos@neu.edu,
                 dcard@uta.edu,
                 schaal.s@husky.neu.edu,
                 wallacewly@gmail.com,
                 grunwaln@science.oregonstate.edu")

URL_MINOTAUR <- list('MINOTAUR' = "https://github.com/NESCent/MINOTAUR")
URL_NESCent <- list("NESCent" = "http://nescent.org/")
URL_Bug <- list("Report Bugs" = "https://github.com/NESCent/MINOTAUR/issues/new")
URL_Contact <- list("Contact Us" =
                      "mailto:caitiecollins@gmail.com,
                    r.verity@imperial.ac.uk,
                    k.lotterhos@neu.edu,
                    dcard@uta.edu,
                    schaal.s@husky.neu.edu,
                    wallacewly@gmail.com,
                    grunwaln@science.oregonstate.edu")

# ---------------------------------------------------------------------------------------------------------------

server <- function(input, output) {

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
                    v<strong>I</strong>sualisatio<strong>N</strong>
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

  #####################
  ## INPUT DATA PAGE ##  ------------------------------------------------------------------------------------
  #####################

  ####################
  ## Box: Load Data ##
  ####################

  # box for loading data
  output$box_loadData <- renderUI({
    box(title='Load Data', status='primary', solidHeader=TRUE, collapsible=TRUE, width=4,
        h3('Load your data object from file'),
        p('Data must be formated as a comma separated file (.csv) or a plain text file (.txt). Headers and delimiters can be specified below'),

        wellPanel(
          fluidRow(
          column(6,
                 p(strong('Headers')),
                 checkboxInput('headerOnCheckBox', label='Use headers', value=TRUE)
          ),
          column(6,
                 p(strong('Delimiters')),
                 radioButtons('delimiters', label=NULL, choices=list('comma-separated'=',', 'tab-separated'='\t', 'space-separated'=' '))
          )
          )
        ),

        fileInput('inputFile',label='Load data from file',accept=c('text/csv','text/plain')),

        hr(),

        h3('Or choose one of our built-in examples'),
        selectInput('exampleData', label='Select example', choices=list('(use own data)'='use_own', 'Large Data'='large_data'), selected='large_data')

    )
  })

  # reactive conductor for reading data from file, or using example data. Returns list(data,name,description,rows,cols)
  rawData <- reactive({
    print(paste('READING DATA FROM FILE',sample(100,1)))
    nullOutput <- list(data=NULL, name=NULL, description=NULL, rows=NULL, cols=NULL)

    # if both read data and example data are NULL (ie. on startup), return nullOutput
    if (is.null(input$inputFile) & is.null(input$exampleData))
      return(nullOutput)

    # if read data is NULL and example data is 'use_own', return nullOutput
    if (is.null(input$inputFile) & input$exampleData=='use_own')
      return(nullOutput)

    # if using example data then this takes precedence over own data
    if (input$exampleData=='large_data') {
      data(largeData, package="MINOTAUR", envir=environment())
      output <- list(data=largeData,
                     name='Example: Large Data',
                     description='This is Liuyang\'s data set that we have been using as an example. Although this is called "Large Data", in fact this is not so large compared with many other data sets!',
                     rows=nrow(largeData),
                     cols=ncol(largeData))
      return(output)
    }

    # by this point we know that the user has chosen to load their own data (all other options are exhausted)
    if (input$inputFile$type%in%c('text/csv','text/plain')) {
      userData <- try(data.frame(fread(input$inputFile$datapath, header=input$headerOnCheckBox, sep=input$delimiters)), silent=TRUE)
      if (class(userData)=='try-error') {
        output <- list(data=NULL,
                       name=input$inputFile$name,
                       description='Error: failed to import data. Check that data is formatted correctly.',
                       rows=NULL,
                       cols=NULL)
        return(output)
      } else {
        output <- list(data=userData,
                       name=input$inputFile$name,
                       description=NULL,
                       rows=nrow(userData),
                       cols=ncol(userData))
        return(output)
      }
    }
  })

  #######################
  ## Box: Data Summary ##
  #######################

  # box for data name (title)
  output$box_dataName <- renderUI({
    box(title='Data Summary', status='warning', solidHeader=TRUE, collapsible=TRUE, width=8,
        h1(rawData()$name),
        p(rawData()$description)
    )
  })

  # valueBox for data rows
  output$valueBox_rows <- renderUI({
    valueBox(value=HTML(paste('<font size=5>rows:  </font> <font size=6>',rawData()$rows,'</font>',sep='')), subtitle='', color='yellow', width=4)
  })

  # valueBox for data cols
  output$valueBox_cols <- renderUI({
    valueBox(value=HTML(paste('<font size=5>columns:  </font> <font size=6>',rawData()$cols,'</font>',sep='')), subtitle='', color='yellow', width=4)
  })

  # box for summarising raw data
  output$box_rawDataSummary <- renderUI({
    box(title='Summary of variables', solidHeader=TRUE, collapsible=TRUE, width=8,
        DT::dataTableOutput("rawDataSummary")
    )
  })

  # raw data summary table
  output$rawDataSummary <- renderDataTable({
    rawData <- rawData()

    # if rawData$data is NULL, return NULL (no table)
    if (is.null(rawData$data))
      return(NULL)

    # count number of NAs
    num.NA <- mapply(FUN=function(x){sum(is.na(x))},rawData$data)

    # produce data frame of summary variables
    output <- data.frame('Variable_Name'=names(rawData$data),
                         'Variable_Class'=mapply(class,rawData$data),
                         'Number_NAs'=num.NA,
                         'Percent_NAs'=paste(round(num.NA/rawData$rows*100,1),'%',sep=''),
                         'Min'=mapply(FUN=function(x){
                           if (is.numeric(x)) {
                             return(min(x,na.rm=TRUE))
                           } else {
                             return(NA)
                           }
                           },rawData$data),
                         'Median'=mapply(FUN=function(x){
                           if (is.numeric(x)) {
                             return(median(x,na.rm=TRUE))
                           } else {
                             return(NA)
                           }
                         },rawData$data),
                         'Mean'=mapply(FUN=function(x){
                           if (is.numeric(x)) {
                             return(mean(x,na.rm=TRUE))
                           } else {
                             return(NA)
                           }
                         },rawData$data),
                         'Max'=mapply(FUN=function(x){
                           if (is.numeric(x)) {
                             return(max(x,na.rm=TRUE))
                           } else {
                             return(NA)
                           }
                         },rawData$data)
                         )
    # output as table
    DT::datatable(output, class='compact', rownames=FALSE, colnames=c('Variable Name', 'Variable Class', 'Number NAs', 'Proportion NAs', 'Min', 'Median', 'Mean', 'Max'), options=list(dom='ltpr'))
  })

  #######################
  ## CLEANUP DATA PAGE ##  ------------------------------------------------------------------------------------
  #######################

  ######################
  ## Box: Subset Data ##
  ######################

  # box for subsetting data
  output$box_subsetData <- renderUI({
    box(title="Subset Data", status="primary", solidHeader=TRUE, collapsible=TRUE, width=12,
        h3('Subset Columns and Remove Missing Data'),
        p('Here you can clean up your data before taking it forward to the next steps.',strong('Do not'),'manually remove outliers at this stage - these will be dealt with later.'),

        h3('Remove Columns'),

        selectizeInput('selectSubsetCols', label=NULL, choices=names(rawData()), multiple=TRUE),

        radioButtons('subsetRemoveOrRetain', label=NULL, choices=list('remove chosen columns'='remove', 'retain chosen columns'='retain')),

        h3('Remove Missing Data'),

        checkboxGroupInput('removeMissing', label=NULL, choices=list('Remove NA'='NA', 'Remove NaN'='NaN', 'Remove non-finite\n(Inf and -Inf)'='non-finite'))
    )
  })

  # reactive conductor for subsetting raw data. Returns list(data,rows,cols,numRemoved,percentRemoved)
  finalData <- reactive({
    nullOutput <- list(data=NULL, rows=NULL, cols=NULL, numRemoved=NULL, percentRemoved=NULL)
    rawData <- rawData()
    print(paste('SUBSETTING DATA',sample(100,1)))

    # if rawData$data is NULL, return nullOutput
    if (is.null(rawData$data))
      return(nullOutput)

    # if remove-or-retain radio button is NULL (ie. on startup), return nullOutput
    if (is.null(input$subsetRemoveOrRetain))
      return(nullOutput)

    # subset columns
    if (input$subsetRemoveOrRetain=='remove') {
      subData <- rawData$data[,which(!names(rawData$data)%in%input$selectSubsetCols),drop=FALSE]
    } else if (input$subsetRemoveOrRetain=='retain') {
      subData <- rawData$data[,which(names(rawData$data)%in%input$selectSubsetCols),drop=FALSE]
    }
    if (ncol(subData)==0)  # if all columns removed
      return(nullOutput)

    # subset rows
    if (!is.null(input$removeMissing)) {
      # remove NA
      if ('NA'%in%input$removeMissing) {
        subData <- subData[rowSums(mapply(is.na,subData))==0,,drop=FALSE]
        if (nrow(subData)==0)  # if all rows removed
          return(nullOutput)
      }
      # remove NaN
      if ('NaN'%in%input$removeMissing) {
        subData <- subData[which(rowSums(mapply(function(x){x=='NaN'},subData),na.rm=T)==0),,drop=FALSE]
        if (nrow(subData)==0)  # if all rows removed
          return(nullOutput)
      }
      # remove non-finite (Inf and -Inf)
      if ('non-finite'%in%input$removeMissing) {
        subData <- subData[which(rowSums(mapply(function(x){x%in%c('Inf','-Inf')},subData),na.rm=T)==0),,drop=FALSE]
        if (nrow(subData)==0)  # if all rows removed
          return(nullOutput)
      }
    }

    # finally return subData
    output <- list(data=subData,
                   rows=nrow(subData),
                   cols=ncol(subData),
                   numRemoved=rawData$rows-nrow(subData),
                   percentRemoved=round((1-nrow(subData)/rawData$rows))*100, digits=10)
    return(output)
  })

  #################################
  ## Box: Missing Data % Removed ##
  #################################

  # valueBox for % missing data removed
  output$valueBox_missingDataRemoved <- renderUI({
    valueBox(value=HTML(paste('<font size=5>rows removed:  </font> <font size=6>',
                              finalData()$numRemoved,
                              ' (',
                              finalData()$percentRemoved,
                              '%)</font>'
                              ,sep='')
    ), subtitle='', color='yellow', width=12)
  })

  ########################
  ## Box: Filtered Data ##
  ########################

  # box for showing filtered data
  output$box_cleanupData <- renderUI({
    box(title="Filtered Data", status="warning", solidHeader=TRUE, collapsible=TRUE, width=12,
        dataTableOutput("cleanupData")
    )
  })

  # filtered data table
  output$cleanupData <- renderDataTable({
    finalData()$data
  },options=list(scrollX=TRUE, scrollY='500px'), rownames=FALSE,
  )


  ########################
  ## FIND OUTLIERS PAGE ##  ------------------------------------------------------------------------------------
  ########################

  #######################################################
  ## Box: Global Controls for Univariate Distributions ##
  #######################################################

  # box containing controls for univariate distribution plots
  output$box_global_univariate <- renderUI({
    box(title="Univariate Distributions", status="primary", solidHeader=TRUE, collapsible=TRUE, width=12,
        fluidRow(
          column(6,
              selectInput('positionVariable', label='Position variable', choices=as.list(c('(none)',names(finalData()$data))))
          ),
          column(6,
              selectInput('chromosomeVariable', label='Chromosom variable', choices=as.list(c('(none)',names(finalData()$data))))
          )
        ),
        fluidRow(
          #useShinyjs(),
          column(12,
                 selectizeInput('univariateVariables', label='Plotting variable(s)', choices=names(finalData()$data), multiple=TRUE),
                 disabled(
                    selectInput('foobar', label='foobar', choices=list('foo','bar'))
                 )
          )
        )
    )
  })

  ###########################################################
  ## Box: Individual Controls for Univariate Distributions ##
  ###########################################################

  # box containing individual controls for univariate distributions
  output$box_controls_univariate <- renderUI({
    box(title="", solidHeader=TRUE, collapsible=TRUE, width=12,
        uiOutput('lapply_controls')
    )
  })

  output$lapply_controls <- renderUI({
    k <- 3
    lapply(1:k,function(i){
      selectInput(paste('foo_',i,sep=''), label=paste('bar',i), choices=list(i))
    })
  })

  ##########################
  ## Box: Navigation Plot ##
  ##########################

  # box for navigation plot
  output$box_navigation <- renderUI({
    tabBox(title="Navigation", width=12,
           tabPanel('(some text)','',
                    plotOutput('plot_navigation',height=100,
                               brush=brushOpts(
                                 id='navigation_brush',
                                 direction='x'
                               )
                    ),
                    sliderInput('slider_navigate',
                                label=NULL,
                                min=navigationPlotParameters()$x_min,
                                max=navigationPlotParameters()$x_max,
                                value=c(navigationPlotParameters()$x_min, navigationPlotParameters()$x_max),
                                step=1,
                                width='100%')
           ) # end of tabPanel
    ) # end of tabBox
  })

  # reactive conductor for calculating plotting parameters. Returns list(x,x_modifier,y,colVec,x_min,x_max)
  navigationPlotParameters <- reactive({
    nullOutput <- list(x=NULL, x_modifier=NULL, y=NULL, colVec=NULL, x_min=0, x_max=100)
    finalData <- finalData()

    # if finalData$data is NULL, return nullOutput
    if (is.null(finalData$data))
      return(nullOutput)

    # if univariateVariables is NULL, return nullOutput
    if (is.null(input$univariateVariables))
      return(nullOutput)

    # find if using position and chromosome variables
    usePosition <- FALSE
    useChromosome <- FALSE
    if (!is.null(input$positionVariable))
      usePosition <- (input$positionVariable!='(none)')
    if (!is.null(input$chromosomeVariable))
      useChromosome <- (input$chromosomeVariable!='(none)')

    # use chromosome variable if present, 1 otherwise
    if (useChromosome) {
      chrom <- finalData$data[,input$chromosomeVariable]
    } else {
      chrom <- rep(1,finalData$rows)
    }

    # use position variable if present, increase linearly over chromosomes if not
    if (usePosition) {
      x <- finalData$data[,input$positionVariable]
    } else {
      chromCounts <- data.frame(table(chrom))
      chromCounts <- chromCounts[match(chromCounts[,1],unique(chrom)),2]
      x <- sequence(chromCounts)
    }

    # calculate position modifier, which ensures that positions increase continuously over genome
    x_list <- split(x,chrom)
    x_maxPerChromosome <- unlist(lapply(x_list,max))
    x_maxIncreasing <- cumsum(x_maxPerChromosome) - x_maxPerChromosome
    x_modifier <- x_maxIncreasing[match(chrom,unique(chrom))]

    # get variable to plot on y-axis
    y <- finalData$data[,input$univariateVariables[1]]

    # convert chromosomes to colours
    colVec <- c(grey(0.8),'#FF9999')[2-chrom%%2]

    # output final parameters
    output <- list(x=x,
                   x_modifier=x_modifier,
                   y=y,
                   colVec=colVec,
                   x_min=min(x+x_modifier),
                   x_max=max(x+x_modifier))
    return(output)
  })

  # navigation plot
  output$plot_navigation <- renderPlot({
    print('PRODUCING NAVPLOT')

    navParams <- navigationPlotParameters()
    if (is.null(navParams$x))
      return(NULL)

    enable('foobar')

    # produce navigation plot
    par(mar=c(0.2,0.2,0.2,0.2))
    plot(navParams$x+navParams$x_modifier, navParams$y, col=navParams$colVec, pch=20, cex=0.7, xaxs='i', yaxs='i', xaxt='n', yaxt='n', ann=FALSE)
    abline(v=input$slider_navigate)
  })

  ###########################
  ## Box: Univariate Plots ##
  ###########################

  # box for plotting univariate distributions
  output$box_plot_univariate <- renderUI({
    tabBox(title="Univariate Plots", width=12,
      tabPanel('(some text)','',
          plotOutput('plot1'),
          h3(input$navigation_brush),
          uiOutput('trick')
      )
    )
  })

  output$trick <- renderUI({
    k <- 3
    lapply(1:k,function(i){
        selectInput('foo', label=paste('bar',i), choices=list(i))
    })
  })

  # example plot
  output$plot1 <- renderPlot({

    navParams <- navigationPlotParameters()
    if (is.null(navParams$x))
      return(NULL)

    plot(navParams$x+navParams$x_modifier, navParams$y, col=navParams$colVec, pch=20, cex=1, xaxs='i', yaxs='i', xlim=c(input$navigation_brush$xmin, input$navigation_brush$xmax))
  })

  ##############################################
  ## Box: Controls for Distance-Based Methods ##
  ##############################################

  # box containing controls for distance-based measure plots
  output$box_control_distanceBased <- renderUI({
    box(title="Distance-Based Outlier Detection", status="warning", solidHeader=TRUE, collapsible=TRUE, width=4,
        h3('something in here')
    )
  })

  ###############################
  ## Box: Distance-Based Plots ##
  ###############################

  # box for distance-based measures (harmonic mean and nearest-neighbour)
  output$box_plot_distanceBased <- renderUI({
    tabBox(title='Distance-Based Methods', width=8,
      tabPanel('Harmonic Mean Distance','',
          plotOutput('plot2')
      ),
      tabPanel('Nearest-Neighbour Distance','',
          p('bloo blah')
      )
    )
  })

  # example plot
  output$plot2 <- renderPlot({
    plot(rnorm(1e3))
  })

  #############################################
  ## Box: Controls for Density-Based Methods ##
  #############################################

  # box containing controls for density-based measure plots
  output$box_control_densityBased <- renderUI({
    box(title="Density-Based Outlier Detection", status="warning", solidHeader=TRUE, collapsible=TRUE, width=4,
        p('this is where I put the details of density-based measures, such as kernel-density with fixed and ML bandwidth.')
    )
  })

  ##############################
  ## Box: Density-Based Plots ##
  ##############################

  # box for density-based measures (kernel density)
  output$box_plot_densityBased <- renderUI({
    tabBox(title="Density-Based Methods", width=8,
      tabPanel('Heuristic Bandwidth','',
               plotOutput('plot3')
      ),
      tabPanel('Maximum-Likelihood Bandwidth','',
          p('begin trick')
      )
    )
  })

  # example plot
  output$plot3 <- renderPlot({
    plot(rnorm(1e3))
  })

  #----------------------------------------------------------------
  # STUFF THAT MAY NOT BE NEEDED
  # load data button
  output$loadDataButton <- renderUI({
    fluidRow(
      column(12,
             div(style="display:inline-block",
                 tags$button(id='loadDataButton', type="button",
                             class="btn action-button btn-primary",
                             style='font-size:15px; text-align:center',
                             HTML('<i class="icon-star"></i>Load Data')
                 ),
                 HTML(paste('<input type="text" value="',rv$data_name,'" readonly="readonly">',sep=''))
             ),
             align='center')
    )
  })
  observeEvent(input$loadDataButton, {
    print("foobar")
    rv$data_name <- rnorm(1)
  })
  rv <- reactiveValues(
    data=NULL,
    data_name=NULL
  )

  output$scratchPad <- renderUI({
    wellPanel(
      h3(is.null(input$inputFile$type)),
      h3(input$inputFile$name),
      h3(input$inputFile$size),
      hr(),
      h3(input$inputFile$type),
      hr(),
      h3(input$inputFile$datapath)
    )
  })
  #----------------------------------------------------------------

}
