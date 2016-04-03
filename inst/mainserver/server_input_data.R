
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
# use_data(OneRefSim, overwrite=TRUE) # , overwrite=TRUE
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
# ##################
# ## 3) HumanGWAS ##
# ##################
# HumanGWAS <- get(load("~/MINOTAUR (copy)/data/largeData.rda"))
# str(HumanGWAS)
# head(HumanGWAS)
# save(HumanGWAS, file="~/MINOTAUR (copy)/data/HumanGWAS.Rdata")
# use_data(HumanGWAS, overwrite=TRUE)
# data("HumanGWAS")

###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###

####################
## Box: Load Data ##
####################

output$headerBox_loadData <- renderUI({
  valueBox(
    subtitle = HTML(paste('<font size=5>Load Data</font>')),
    color = "light-blue",
    value = NULL,
    width=12
  )
})

# box for loading data
output$tabBox_loadData <- renderUI({

  tabBox(width=12,
         status='warning',

      ##################
      ## LOAD EXAMPLE ##
      ##################
      tabPanel(title=HTML('<font size=4>Example Data</font>'), icon=icon("bar-chart"),
              h3('Work with example data'),
              selectInput('exampleData', label='Select example',
                            choices=list("(use own data)" = "use_own",
                                         "Expansion from One Refugia" = "OneRefSim",
                                         "Human GWAS" = "HumanGWAS",
                                         "Non-Parametric Data" = "NonParamEx"), selected="OneRefSim")
      ),


      #########################
      ## LOAD DATA FROM FILE ##
      #########################
      tabPanel(title=HTML('<font size=4>Upload Data</font>'), icon=icon("upload"),
              h3('Upload data from file'),
              p('Data must be formated as a comma separated file (.csv) or a plain text file (.txt).
                Headers and delimiters can be specified below'),

                    wellPanel(
                      fluidRow(
                        column(6,
                               p(strong('Headers')),
                               checkboxInput('headerOnCheckBox',
                                             label='Use headers',
                                             value=TRUE)
                        ),
                        column(6,
                               p(strong('Delimiters')),
                               radioButtons('delimiters',
                                            label=NULL,
                                            choices=list('comma-separated'=',',
                                                         'tab-separated'='\t',
                                                         'space-separated'=' '))
                        )
                      ), style='padding: 10px;'
                    ),

              fileInput('inputFile',label='Load data from file',accept=c('text/csv','text/plain')),
              #, "RData","Rdata","Rda","RDA", "rdata", "rda"
              hr()
              )

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
  ## OneRefSim ##
  if (input$exampleData=='OneRefSim') {
    data(OneRefSim, package="MINOTAUR", envir=environment())
    output <- list(data=OneRefSim,
                   name='Example: Simulated Expansion from One Refugia',
                   description='This is Katie\'s data set that contains population genetic data simulating expansion from one refugia.
                   NOTE-- TO BE CHANGED TO TwoRefSim, the Expansion from TWO Refugia simulated dataset.',
                   rows=nrow(OneRefSim),
                   cols=ncol(OneRefSim))
    return(output)
  }
  ## HumanGWAS ##
  if (input$exampleData=='HumanGWAS') {
    data(HumanGWAS, package="MINOTAUR", envir=environment())
    output <- list(data=HumanGWAS,
                   name='Example: Human GWAS',
                   description='This is Liuyang\'s data set that contains an example of output returned from a human GWAS analysis.',
                   rows=nrow(HumanGWAS),
                   cols=ncol(HumanGWAS))
    return(output)
  }
  ## NonParamEx ##
  if (input$exampleData=='NonParamEx') {
    data(NonParamEx, package="MINOTAUR", envir=environment())
    output <- list(data=NonParamEx,
                   name='Example: Non-Parametric Data',
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
  valueBox(value=HTML(paste('<font size=5>rows:  </font> <font size=6>',
                            rawData()$rows,'</font>',sep='')),
           subtitle='', color='yellow', width=6)
})

# valueBox for data cols
output$valueBox_cols <- renderUI({
  valueBox(value=HTML(paste('<font size=5>columns:  </font> <font size=6>',
                            rawData()$cols,'</font>',sep='')),
           subtitle='', color='yellow', width=6)
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

# ## Example: Colour cells


#############
# # raw data table (original)
# output$rawDataTable <- renderDataTable({
#   rawData()$data
# },options=list(scrollX=TRUE, scrollY='400px') #, rownames=FALSE
# )
#############

# raw data table
options(DT.options = list(scrollX=TRUE, scrollY='400px'))#, rownames=FALSE

## output as table (example) ##
## example: coloured cells

## REQUIRES THE GITHUB VERSION OF DT!!!!!!!
## NEED To MAKE SURE WE CAN RELEASE/REQUIRE THIS VERSION WITH/IN THE PACKAGE!!!!
#devtools::install_github('rstudio/DT')

output$rawDataTable <- DT::renderDataTable({

  ## Get data:
  dat <- rawData()$data

  ## For the moment, I'm only using the first 200 rows for this example.
  ## Gets slow by 1,000 rows and runs into an error w full dataset...
  dat <- dat[c(1:200),]

  #set.seed(1)
  #dat <- matrix(sample(c(1:20), 500, replace=TRUE), nrow=50, byrow=TRUE)

  if(!is.null(dat)){

    ## Select colours for levels of table cells to be coloured.
    levs <- myCol <- style <- list()

    #par(mfrow=c(1,5))

    ## for loop to get levels and colour schemes for each column of dat:
    for(i in 1:ncol(dat)){

      levs[[i]] <- list()
      #require(adegenet)
      #levs[[i]] <- levels(as.factor(dat[,i]))
      levs[[i]][[1]] <- unique(dat[,i])
      ## if levs contains no numbers, convert to numeric:

      ## When using formatStyle to colour table later,
      ## we will need levels sorted as numeric or as character:
      #require(Hmisc)
      if(all.is.numeric(levs[[i]][[1]])){
        levs[[i]][[2]] <- as.numeric(levs[[i]][[1]])
        ## remove highest level??
        #levs[[i]][[2]] <- levs[[i]][[2]][-which(levs[[i]][[2]] == levs[[i]][[2]][which.max(levs[[i]][[2]])])]
      }else{
        levs[[i]][[2]] <- as.character(levs[[i]][[1]])
      }
      levs[[i]][[2]] <- sort(levs[[i]][[2]])

      ## For colour scheme, we need numeric levels:
      levs[[i]][[1]] <- as.numeric(as.factor(levs[[i]][[1]]))

      ## NOTE: Could change this to check before for loop if we wanted
      ## to stick w ONE colour scheme throughout table, regardless of levels in each column
      ## (eg. to use heat.colors if any column has > 20 unique values/levels)...
      if(length(levs[[i]][[1]]) <= 20){
        ## Can use adegenet colour palettes for factors w < 20 levels
        if(all.is.numeric(levs[[i]][[2]])){
          myCol[[i]] <- funky(n=length(levs[[i]][[1]])+1)
        }else{
          myCol[[i]] <- funky(n=length(levs[[i]][[1]]))
        }
        ## Add transparency? ##
        ## NOTE: transparency not working for background colours in table cells.
        ## Would be nice to find a way to add some because the colours are a little blunt as is...
        #myCol <- transp(myCol, 0.5)
      }else{
        ## Could use heat.colorsfor "factors" w Inf levels...
        if(all.is.numeric(levs[[i]][[2]])){
          myCol[[i]] <- heat.colors(n=length(levs[[i]][[1]])+1)
        }else{
          myCol[[i]] <- heat.colors(n=length(levs[[i]][[1]]))
        }
        ## , alpha=0.8 # to add transparency (not working)
        ## NOTE: Because heat.colors automatically adds the last two "transparency" characters
        ## to the character strings it generates to specify colours, we need to remove these (they are/must be
        ## NULL anyway as transparency not working in tables), which can be done w a utils.R fn I borrowed from treeWAS.
        myCol[[i]] <- .removeLastN(myCol[[i]], 2)
      }
      ## (temp:) check out colours in console
      #barplot(rep(10, length(myCol[[i]])), col=myCol[[i]])

      #       ## NOTE: heat.colors has a maximum n.levels of 511.
      #       ## For any styleInterval w > 512 levels, need to set cut points appropriately:
      #       if(length(levs[[i]][[2]]) > 512){
      #         levs[[i]][[2]] <- levs[[i]][[2]][which(duplicated(myCol[[i]])==FALSE)]
      #         myCol[[i]] <- unique(myCol[[i]])
      #       }


      ## get formatStyle expression for this column:
      if(all.is.numeric(levs[[i]][[2]])){
        style[[i]] <- paste("formatStyle(",
                            i,
                            ", target='cell', backgroundColor = styleInterval(levs[[",
                            i,
                            "]][[2]], myCol[[",
                            i,
                            "]]))", sep="")
      }else{
        style[[i]] <- paste("formatStyle(",
                            i,
                            ", target='cell', backgroundColor = styleEqual(levs[[",
                            i,
                            "]][[2]], myCol[[",
                            i,
                            "]]))", sep="")
      }
      #print(style[[i]])
    }

    ## temp:
    #     ## check length of elements of levs
    #     lev.l <- sapply(c(1:length(levs)), function(e) lapply(levs[[e]], length))
    #     ## check length of elements of myCol
    #     col.l <- lapply(myCol, length)
    #     df <- rbind(col.l, lev.l)

    ## collapse elements of style together w %>%
    paste(style, collapse=" %>% ")
    ## paste datatable fn to elements of style w %>%
    out <- paste("datatable(dat)", paste(style, collapse=" %>% "), sep=" %>% ")

    ## render & run expression
    eval(parse(text=out))

  } # end null check

})


#############

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

  ## output as table ##

  DT::datatable(output,
                class='compact',
                # rownames=FALSE,
                colnames=c('Variable Name', 'Variable Class',
                           'Number NAs', 'Proportion NAs',
                           'Min', 'Median', 'Mean', 'Max'),
                options=list(dom='ltpr')
                )


})

