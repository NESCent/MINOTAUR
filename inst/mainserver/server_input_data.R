
#####################
## INPUT DATA PAGE ##  ------------------------------------------------------------------------------------
#####################

##############
## EXAMPLES ##   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###
##############

# ##################
# ## 1) OneRefSim ##
# ##################
#
# OneRefSim <- read.table("~/MINOTAUR (copy)/data/OneRefSim.txt", header=TRUE)
# str(OneRefSim)
# head(OneRefSim)
# save(OneRefSim, file="~/MINOTAUR (copy)/data/OneRefSim.Rdata")
# library(devtools)
# use_data(OneRefSim) # , overwrite=TRUE
# data("OneRefSim")
#
# ###################
# ## 2) NonParamEx ##
# ###################
# NonParamEx <- read.table("~/MINOTAUR (copy)/data/NonParamEx.txt", header=TRUE)
# str(NonParamEx)
# head(NonParamEx)
# save(NonParamEx, file="~/MINOTAUR (copy)/data/NonParamEx.Rdata")
# use_data(NonParamEx, overwrite=TRUE)
# data("NonParamEx")
#
# ###################
# ## 2) NonParamEx ##
# ###################
# NonParamEx <- read.table("~/MINOTAUR (copy)/data/NonParamEx.txt", header=TRUE)
# str(NonParamEx)
# head(NonParamEx)
# save(NonParamEx, file="~/MINOTAUR (copy)/data/NonParamEx.Rdata")
# use_data(NonParamEx, overwrite=TRUE)
# data("NonParamEx")
#
# ################
# ## 3) ToyGWAS ##
# ################
# ToyGWAS <- get(load("~/MINOTAUR (copy)/data/largeData.rda"))
# str(ToyGWAS)
# head(ToyGWAS)
# save(ToyGWAS, file="~/MINOTAUR (copy)/data/ToyGWAS.Rdata")
# use_data(ToyGWAS, overwrite=TRUE)
# data("ToyGWAS")

###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###

####################
## Box: Load Data ##
####################

# box for loading data
output$box_loadData <- renderUI({
  box(title='Load Data', status='primary', solidHeader=TRUE, collapsible=FALSE, width=12,
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
        ), style='padding: 10px;'
      ),

      fileInput('inputFile',label='Load data from file',accept=c('text/csv','text/plain')),
      #, "RData","Rdata","Rda","RDA", "rdata", "rda"

      hr(),

      h3('Or choose one of our built-in examples'),
      selectInput('exampleData', label='Select example',
                  choices=list('(use own data)'='use_own',
                               "Toy GWAS" = "ToyGWAS",
                               "Expansion from One Refugia" = "OneRefSim",
                               "Non-Parametric Data" = "NonParamEx"), selected="ToyGWAS")

  )
})

# reactive conductor for reading data from file, or using example data. Returns list(data,name,description,rows,cols)
rawData <- reactive({
  nullOutput <- list(data=NULL, name=NULL, description=NULL, rows=NULL, cols=NULL)

  # if both user data and example data are NULL (ie. on startup), return nullOutput
  if (is.null(input$inputFile) & is.null(input$exampleData))
    return(nullOutput)

  # if user data is NULL and example data is 'use_own', return nullOutput
  if (is.null(input$inputFile) & input$exampleData=='use_own')
    return(nullOutput)

  # if using example data then this takes precedence over user data
  ## ToyGWAS ##
  if (input$exampleData=='ToyGWAS') {
    data(ToyGWAS, package="MINOTAUR", envir=environment())
    output <- list(data=ToyGWAS,
                   name='Example: Toy GWAS',
                   description='This is Liuyang\'s data set that contains an example of output returned from a human GWAS analysis.',
                   rows=nrow(ToyGWAS),
                   cols=ncol(ToyGWAS))
    return(output)
  }
  ## OneRefSim ##
  if (input$exampleData=='OneRefSim') {
    data(OneRefSim, package="MINOTAUR", envir=environment())
    output <- list(data=OneRefSim,
                   name='Example: Simulated Expansion from One Refugia',
                   description='This is Katie\'s data set that contains population genetic data simulating expansion from one refugia.',
                   rows=nrow(OneRefSim),
                   cols=ncol(OneRefSim))
    return(output)
  }
  ## NonParamEx ##
  if (input$exampleData=='NonParamEx') {
    data(NonParamEx, package="MINOTAUR", envir=environment())
    output <- list(data=NonParamEx,
                   name='Example: Non-Parametric Example',
                   description='This is a simple two-variable data set that contains an example of non-parametric data...
                   (Special use for this data set? Any additional advice needed here??).',
                   rows=nrow(NonParamEx),
                   cols=ncol(NonParamEx))
    return(output)
  }

  # by this point we know that the user has chosen to load their own data (all other options are exhausted)
  if (input$inputFile$type%in%c('text/csv','text/plain')) {
    userData <- try(data.frame(fread(input$inputFile$datapath, header=input$headerOnCheckBox, sep=input$delimiters)), silent=TRUE)
    if (class(userData)=='try-error') {
      print(userData)
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
  box(title='Data Summary', status='warning', solidHeader=TRUE, collapsible=TRUE, width=12,
      h2(rawData()$name),
      HTML(paste('<i><font size=3>',rawData()$description,'</font></i>',sep=''))
  )
})

# valueBox for data rows
output$valueBox_rows <- renderUI({
  valueBox(value=HTML(paste('<font size=5>rows:  </font> <font size=6>',rawData()$rows,'</font>',sep='')), subtitle='', color='yellow', width=6)
})

# valueBox for data cols
output$valueBox_cols <- renderUI({
  valueBox(value=HTML(paste('<font size=5>columns:  </font> <font size=6>',rawData()$cols,'</font>',sep='')), subtitle='', color='yellow', width=6)
})

# tabBox for displaying raw data and data summary
output$tabBox_rawDataSummary <- renderUI({
  tabBox(title=NULL, status='warning', width=12,
         tabPanel(title=HTML('<font size=4>Raw data table</font>'),
                  dataTableOutput("rawDataTable")
         ),
         tabPanel(title=HTML('<font size=4>Summary table</font>'),
                  DT::dataTableOutput("rawDataSummary")
         )
  )
})

# raw data table
output$rawDataTable <- renderDataTable({
  rawData()$data
},options=list(scrollX=TRUE, scrollY='400px') #, rownames=FALSE
)

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
  DT::datatable(output, class='compact', # rownames=FALSE,
                colnames=c('Variable Name', 'Variable Class', 'Number NAs', 'Proportion NAs', 'Min', 'Median', 'Mean', 'Max'), options=list(dom='ltpr'))
})
