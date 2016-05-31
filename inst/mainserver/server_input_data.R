
#####################
## INPUT DATA PAGE ##  ------------------------------------------------------------------------------------
#####################

# ##############
# ## EXAMPLES ##   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###
# ##############
#
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
# ##################
# ## 1) TwoRefSim ##
# ##################
#
# TwoRefSim <- read.table("~/MINOTAUR/data/TwoRefSimForShinyMCD.txt", header=TRUE)
# str(TwoRefSim)
# head(TwoRefSim)
# save(TwoRefSim, file="~/MINOTAUR/data/TwoRefSim.Rdata")
# use_data(TwoRefSim, overwrite=TRUE) # , overwrite=TRUE
# data("TwoRefSim")
#
#
# ###################
# ## 2) NonParamEx ##
# ###################
#
# ## (A) ##
# NonParamEx <- read.table("~/MINOTAUR (copy)/data/NonParamEx.txt", header=TRUE)
# str(NonParamEx)
# head(NonParamEx)
# save(NonParamEx, file="~/MINOTAUR (copy)/data/NonParamEx.Rdata")
# use_data(NonParamEx, overwrite=TRUE)
# data("NonParamEx")

# ## (B) ## Inverse
# library(data.table)
# library(devtools)
# NonParametricInverse <- fread("~/MINOTAUR/data/df_inverse.csv", header=TRUE)
# str(NonParametricInverse)
# head(NonParametricInverse)
# save(NonParametricInverse, file="~/MINOTAUR/data/NonParametricInverse.Rdata")
# use_data(NonParametricInverse, overwrite=TRUE)
# data("NonParametricInverse")
#
# ## (C) ## Multimodal
# NonParametricMultimodal <- fread("~/MINOTAUR/data/df_multimodal.csv", header=TRUE)
# str(NonParametricMultimodal)
# head(NonParametricMultimodal)
# save(NonParametricMultimodal, file="~/MINOTAUR/data/NonParametricMultimodal.Rdata")
# use_data(NonParametricMultimodal, overwrite=TRUE)
# data("NonParametricMultimodal")
#
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

# header box
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

  tabBox(id = "tabSet_loadData",
         width=12,
         status='warning',

         ##################
         ## LOAD EXAMPLE ##
         ##################
         tabPanel(value = "eg", title = HTML('<font size=4>Example Data</font>'), icon = icon("bar-chart"),
                  h3('Work with example data'),
                  selectInput('exampleData', label='Select example',
                              choices=list("(use own data)" = "use_own",
                                           "Human GWAS" = "HumanGWAS",
                                           "Expansion from Two Refugia" = "TwoRefSim",
                                           "Non-Parametric Inverse" = "NonParametricInverse",
                                           "Non-Parametric Multimodal" = "NonParametricMultimodal"),
                              selected="HumanGWAS")
         ),


         #########################
         ## LOAD DATA FROM FILE ##
         #########################
         tabPanel(value = "user",
                  title = HTML('<font size=4>Upload Data</font>'),
                  icon = icon("upload"),

                  h3('Upload data from file'),
                  p('Data must be formated as a comma separated file (.csv) or a plain text file (.txt).
                    Headers and delimiters can be specified below'),

                  fileInput('inputFile', label='Load data from file', accept=NULL),
                  # fileInput('inputFile',label='Load data from file',accept=c('text/csv','text/plain')),
                  # "application/x-r-data"
                  #, "RData","Rdata","Rda","RDA", "rdata", "rda"
                  hr(),

                  ## make CSV-related options a conditional panel, to appear only if file type is NOT Rdata:
                  conditionalPanel("output.userInputCSV == true",
                                   #input$inputFile$type %in% "application/x-r-data"
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
                                   )
                  )
                  )

  )
})


## Dummy output to tell conditionalPanel whether data uploaded is Rdata (ie. is NOT CSV)
output$userInputCSV <- reactive({
  out <- FALSE
  if(!is.null(input$inputFile)){
    if(!is.null(input$inputFile$type)){
      if (!input$inputFile$type %in% "application/x-r-data") {
        out <- TRUE
      }
    }
  }
  return(out)
})
outputOptions(output, "userInputCSV", suspendWhenHidden=FALSE)


## reactive conductor for reading data from file, or using example data.
## Returns list(data,name,description,rows,cols)
rawData <- reactive({

  output <- NULL

  if(is.null(input$tabSet_loadData)){
    ## if no tab panel yet created (ie. on start-up),
    ## (and no input of example data has been selected -- if this is even possible),
    ## automatically load the HumanGWAS example dataset:
    if (is.null(input$inputFile) & is.null(input$exampleData)){
      ## HumanGWAS ##
      data(HumanGWAS, package="MINOTAUR", envir=environment())
      output <- list(data=HumanGWAS,
                     name='Example: Human GWAS',
                     description="This data set contains an example of output returned from a human GWAS analysis (unpublished). The 'Chr' variable gives the chromosome and 'BP' the genomic position. Beta values come from a regression of phenotype against genotype, p-values come from a separate analysis and do not correspond directly to Beta values.",
                     rows=nrow(HumanGWAS),
                     cols=ncol(HumanGWAS))
    }

  }else{
    ## If the tabset panel has been created, choose either eg or user data to load:

    # if using example data...
    if(input$tabSet_loadData == "eg"){
      ## HumanGWAS ##
      if (input$exampleData=='HumanGWAS') {
        data(HumanGWAS, package="MINOTAUR", envir=environment())
        HumanGWAS <- as.data.frame(HumanGWAS)
        output <- list(data=HumanGWAS,
                       name='Example: Human GWAS',
                       description="This data set contains an example of output returned from a human GWAS analysis (unpublished). The 'Chr' variable gives the chromosome and 'BP' the genomic position. Beta values come from a regression of phenotype against genotype, p-values come from a separate analysis and do not correspond directly to Beta values.",
                       rows=nrow(HumanGWAS),
                       cols=ncol(HumanGWAS))
      }
      ## TwoRefSim ##
      if (input$exampleData=='TwoRefSim') {
        data(TwoRefSim, package="MINOTAUR", envir=environment())
        TwoRefSim <- as.data.frame(TwoRefSim)
        output <- list(data=TwoRefSim,
                       name='Example: Simulated Expansion from Two Refugia',
                       description="This data set contains population genetic data simulating expansion from two refugia.",
                       rows=nrow(TwoRefSim),
                       cols=ncol(TwoRefSim))
      }
      ## NonParametricInverse ##
      if (input$exampleData=='NonParametricInverse') {
        data(NonParametricInverse, package="MINOTAUR", envir=environment())
        NonParametricInverse <- as.data.frame(NonParametricInverse)
        output <- list(data=NonParametricInverse,
                       name='Example: Non-Parametric Inverse',
                       description="This is a simulated two-variable dataset. The two variables follow an inverse relationship, with some additional noise.",
                       rows=nrow(NonParametricInverse),
                       cols=ncol(NonParametricInverse))
      }
      ## NonParametricMultimodal ##
      if (input$exampleData=='NonParametricMultimodal') {
        data(NonParametricMultimodal, package="MINOTAUR", envir=environment())
        NonParametricMultimodal <- as.data.frame(NonParametricMultimodal)
        output <- list(data=NonParametricMultimodal,
                       name='Example: Non-Parametric Multimodal',
                       description="This is a simulated two-variable dataset. The data were drawn from a bivariate normal mixture model, resulting in multiple distinct peaks in the distribution.",
                       rows=nrow(NonParametricMultimodal),
                       cols=ncol(NonParametricMultimodal))
      }
    } # end eg input selected



    ## if the user has chosen to load their own data
    if(input$tabSet_loadData == "user"){

      ## If no data yet loaded, print initial message:
      output <- list(data=NULL,
                     name=NULL,
                     description="To upload your own data, click on the 'Choose file' button located in the panel at left.",
                     rows=NULL,
                     cols=NULL)

      if(!is.null(input$inputFile)){
        if(!is.null(input$inputFile$type)){
          ###################
          ## Load if Rdata ##
          ###################
          if (input$inputFile$type %in% "application/x-r-data") {
            userData <- try(get(load(input$inputFile$datapath)), silent=TRUE)
            if (class(userData)=='try-error') {
              print(head(userData))
              output <- list(data=NULL,
                             name=input$inputFile$name,
                             description='Error: failed to import data. Check that data is formatted correctly.',
                             rows=NULL,
                             cols=NULL)
            } else {
              output <- list(data=userData,
                             name=input$inputFile$name,
                             description=NULL,
                             rows=nrow(userData),
                             cols=ncol(userData))
            }
          }else{
            #######################
            ## load if CSV-type: ##
            #######################
            userData <- try(data.frame(fread(input$inputFile$datapath, header=input$headerOnCheckBox, sep=input$delimiters)), silent=TRUE)
            if (class(userData)=='try-error') {
              print(userData)
              output <- list(data=NULL,
                             name=input$inputFile$name,
                             description='Error: failed to import data. Check that data is formatted correctly.',
                             rows=NULL,
                             cols=NULL)
            }else {
              output <- list(data=userData,
                             name=input$inputFile$name,
                             description=NULL,
                             rows=nrow(userData),
                             cols=ncol(userData))
            }
          }

        }else{
          output <- list(data=NULL,
                         name=input$inputFile$name,
                         description='Error: failed to import data. Check that file type is one of: CSV, plain text, or Rdata.',
                         rows=NULL,
                         cols=NULL)
        }
      } # end check for inputFile
    } # end user input selected
  } # end check for tabSet loaded
  
  return(output)
})

#######################
## Box: Data Summary ##
#######################

# box for data name (title)
output$box_dataName <- renderUI({
  box(title='Data Summary', status='warning', solidHeader=TRUE, collapsible=FALSE, width=12,
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
                  #dataTableOutput("rawDataTable")
                  DT::dataTableOutput("rawDataTable")
                  # tableOutput("rawDataTable2")
         ),
         tabPanel(title=HTML('<font size=4>Summary table</font>'),
                  DT::dataTableOutput("rawDataSummary")
         )
  )
})

# raw data table
output$rawDataTable <- DT::renderDataTable({

  out <- NULL

  # get data
  dat <- rawData()$data
  if(!is.null(dat)){
    out <- datatable(dat)
  }

  return(out)
})


# raw data summary table
output$rawDataSummary <- DT::renderDataTable({
  
  # if rawData$data is NULL, return NULL (no table)
  if (is.null(rawData()$data))
    return(NULL)
  
  # count number of NAs
  num.NA <- mapply(FUN=function(x){sum(is.na(x))},rawData()$data)
  
  # produce data frame of summary variables
  output <- data.frame('Variable_Name'=names(rawData()$data),
                       'Variable_Class'=mapply(class,rawData()$data),
                       'Number_NAs'=num.NA,
                       'Percent_NAs'=paste(round(num.NA/rawData()$rows*100,1),'%',sep=''),
                       'Min'=mapply(FUN=function(x){
                         if (is.numeric(x)) {
                           return(signif(min(x,na.rm=TRUE),3))
                         } else {
                           return(NA)
                         }
                       },rawData()$data),
                       'Median'=mapply(FUN=function(x){
                         if (is.numeric(x)) {
                           return(signif(median(x,na.rm=TRUE),3))
                         } else {
                           return(NA)
                         }
                       },rawData()$data),
                       'Mean'=mapply(FUN=function(x){
                         if (is.numeric(x)) {
                           return(signif(mean(x,na.rm=TRUE),3))
                         } else {
                           return(NA)
                         }
                       },rawData()$data),
                       'Max'=mapply(FUN=function(x){
                         if (is.numeric(x)) {
                           return(signif(max(x,na.rm=TRUE),3))
                         } else {
                           return(NA)
                         }
                       },rawData()$data)
  )
  
  # output as table
  DT::datatable(output,
                class='compact',
                rownames=FALSE,
                colnames=c('Variable Name', 'Variable Class',
                           'Number NAs', 'Proportion NAs',
                           'Min', 'Median', 'Mean', 'Max'),
                options=list(dom='ltpr')
  )
  
  
})



############## Example: Colour cells - ARCHIVED FOR NOW DUE TO DIFFICULTIES WORKING WITH LARGE TABLES

# # raw data table
# options(DT.options = list(scrollX=TRUE, scrollY='400px'))#, rownames=FALSE
#
# ## output as table (example) ##
# ## example: coloured cells
#
# ## REQUIRES THE GITHUB VERSION OF DT!!!!!!!
# ## NEED To MAKE SURE WE CAN RELEASE/REQUIRE THIS VERSION WITH/IN THE PACKAGE!!!!
# #devtools::install_github('rstudio/DT')
#
# output$rawDataTable <- DT::renderDataTable({
#
#   out <- NULL
#
#   ## Get data:
#   dat <- rawData()$data
#
#   #set.seed(1)
#   #dat <- matrix(sample(c(1:20), 500, replace=TRUE), nrow=50, byrow=TRUE)
#   if(!is.null(dat)){
#
#     ## For the moment, I'm only using the first 200 rows for this example.
#     ## Gets slow by 1,000 rows and runs into an error w full dataset...
#     dat <- dat[c(1:200),]
#
#     ## try to render & run expression
#     try.out <- try(.get.colorTable.style(dat), silent=TRUE)
#
#     if(class(try.out) != "try-error"){
#       temp <- .get.colorTable.style(dat) # try.out
#       style <- temp$style
#       levs <- temp$levs
#       myCol <- temp$myCol
#       ## collapse elements of style together w %>%
#       paste(style, collapse=" %>% ")
#       ## paste datatable fn to elements of style w %>%
#       out <- paste("datatable(dat)", paste(style, collapse=" %>% "), sep=" %>% ")
#       out <- eval(parse(text=out))
#       # out <- dataTableOutput(out)
#     }else{
#       #       rawDT.error <- "Error: Data table could not be generated from file.
#       #                   Check that the appropriate controls have been selected in the panel at left
#       #                   and that the file is in the right format."
#       #       print(rawDT.error)
#       #       # out <- textOutput(print(rawDT.error))
#       #       out <- rawDT.error
#       out <- NULL
#     }
#
#   } # end null check
#
#   return(out)
# })
#
#
#
# ###########################
# ## .get.colorTable.style ##
# ###########################
# .get.colorTable.style <- function(dat){
#
#   if(!is.null(dat)){
#
#     ## Select colours for levels of table cells to be coloured.
#     levs <- myCol <- style <- list()
#
#     #par(mfrow=c(1,5))
#
#     ## for loop to get levels and colour schemes for each column of dat:
#     for(i in 1:ncol(dat)){
#
#       levs[[i]] <- list()
#       #require(adegenet)
#       #levs[[i]] <- levels(as.factor(dat[,i]))
#       levs[[i]][[1]] <- unique(dat[,i])
#       ## if levs contains no numbers, convert to numeric:
#
#       ## When using formatStyle to colour table later,
#       ## we will need levels sorted as numeric or as character:
#       #require(Hmisc)
#       if(all.is.numeric(levs[[i]][[1]])){
#         levs[[i]][[2]] <- as.numeric(levs[[i]][[1]])
#         ## remove highest level??
#         #levs[[i]][[2]] <- levs[[i]][[2]][-which(levs[[i]][[2]] == levs[[i]][[2]][which.max(levs[[i]][[2]])])]
#       }else{
#         levs[[i]][[2]] <- as.character(levs[[i]][[1]])
#       }
#       levs[[i]][[2]] <- sort(levs[[i]][[2]])
#
#       ## For colour scheme, we need numeric levels:
#       levs[[i]][[1]] <- as.numeric(as.factor(levs[[i]][[1]]))
#
#       ## NOTE: Could change this to check before for loop if we wanted
#       ## to stick w ONE colour scheme throughout table, regardless of levels in each column
#       ## (eg. to use heat.colors if any column has > 20 unique values/levels)...
#       if(length(levs[[i]][[1]]) <= 20){
#         ## Can use adegenet colour palettes for factors w < 20 levels
#         if(all.is.numeric(levs[[i]][[2]])){
#           myCol[[i]] <- funky(n=length(levs[[i]][[1]])+1)
#         }else{
#           myCol[[i]] <- funky(n=length(levs[[i]][[1]]))
#         }
#         ## Add transparency? ##
#         ## NOTE: transparency not working for background colours in table cells.
#         ## Would be nice to find a way to add some because the colours are a little blunt as is...
#         #myCol <- transp(myCol, 0.5)
#       }else{
#         ## Could use heat.colorsfor "factors" w Inf levels...
#         if(all.is.numeric(levs[[i]][[2]])){
#           myCol[[i]] <- heat.colors(n=length(levs[[i]][[1]])+1)
#         }else{
#           myCol[[i]] <- heat.colors(n=length(levs[[i]][[1]]))
#         }
#         ## , alpha=0.8 # to add transparency (not working)
#         ## NOTE: Because heat.colors automatically adds the last two "transparency" characters
#         ## to the character strings it generates to specify colours, we need to remove these (they are/must be
#         ## NULL anyway as transparency not working in tables), which can be done w a utils.R fn I borrowed from treeWAS.
#         myCol[[i]] <- .removeLastN(myCol[[i]], 2)
#       }
#       ## (temp:) check out colours in console
#       #barplot(rep(10, length(myCol[[i]])), col=myCol[[i]])
#
#       #       ## NOTE: heat.colors has a maximum n.levels of 511.
#       #       ## For any styleInterval w > 512 levels, need to set cut points appropriately:
#       #       if(length(levs[[i]][[2]]) > 512){
#       #         levs[[i]][[2]] <- levs[[i]][[2]][which(duplicated(myCol[[i]])==FALSE)]
#       #         myCol[[i]] <- unique(myCol[[i]])
#       #       }
#
#
#       ## get formatStyle expression for this column:
#       if(all.is.numeric(levs[[i]][[2]])){
#         style[[i]] <- paste("formatStyle(",
#                             i,
#                             ", target='cell', backgroundColor = styleInterval(levs[[",
#                             i,
#                             "]][[2]], myCol[[",
#                             i,
#                             "]]))", sep="")
#       }else{
#         style[[i]] <- paste("formatStyle(",
#                             i,
#                             ", target='cell', backgroundColor = styleEqual(levs[[",
#                             i,
#                             "]][[2]], myCol[[",
#                             i,
#                             "]]))", sep="")
#       }
#       #print(style[[i]])
#     }
#     ## temp:
#     #     ## check length of elements of levs
#     #     lev.l <- sapply(c(1:length(levs)), function(e) lapply(levs[[e]], length))
#     #     ## check length of elements of myCol
#     #     col.l <- lapply(myCol, length)
#     #     df <- rbind(col.l, lev.l)
#   }
#
#   temp <- list(style=style,
#                levs=levs,
#                myCol=myCol)
#
#   return(temp)
#
# } # end .get.colorTable.style

#############

