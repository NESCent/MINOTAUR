
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
