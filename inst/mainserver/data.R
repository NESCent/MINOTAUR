######################
## EXAMPLE DATASETS ##
######################

# ##############################
# ## expl #1 (smallData (??)) ##
# ##############################
# smallData <- read.csv("./misc/Wallace_etal_2014_PLoSGenet_GWAS_hits-150112.csv")
# 
# # str(smallData)
# 
# save(smallData, file="./pkg/data/smallData.Rdata")
# write.csv(smallData, file="./pkg/data/smallData.csv")
# # require("XLConnect")
# writeWorksheetToFile(file="./pkg/data/smallData.xlsx", data = smallData, sheet="Sheet1")
# 
# 
# ##############################
# ## expl #2 (largeData (??)) ##
# ##############################
# largeData <- read.table("./misc/mytoys.txt", header=TRUE)
# 
# save(largeData, file="./pkg/data/largeData.Rdata")
# write.csv(largeData, file="./pkg/data/largeData.csv")
# #require("XLConnect")
# writeWorksheetToFile(file="./pkg/data/largeData.xlsx", data = largeData, sheet="Sheet1")






######################
## .read.input.data ##
######################

## INTERNAL FN TO GET DATA ##

.read.input.data <- function(input){
  
  ## Data sources: (i) example, (ii) df as .Rdata, (iii) df as Excel file
  
  out <- NULL
  
  require("data.table")
  
  #############
  ## EXAMPLE ##
  #############
  if(input$datatype=="eg"){                
    out <- NULL
    if(input$egData=="smallData"){
      ## TEMPORARY VERSION (while pkg not compiled)
      ## these files are TEMPORARILY located in /inst/mainserver
      #out <- get(load("smallData.Rdata")) 
      ## REAL VERSION (pkg must be compiled)
      ## these files will be PERMANENTLY located in /data/
      data(smallData, package="MINOTAUR", envir=environment()) 
    }
    if(input$egData=="largeData"){ 
      ## TEMPORARY VERSION (while pkg not compiled)
      ## these files are TEMPORARILY located in inst/mainserver
      #out <- get(load("largeData.Rdata")) 
      ## REAL VERSION (pkg must be compiled)
      ## these files will be PERMANENTLY located in /data/
      data(largeData, package="MINOTAUR", envir=environment())
    }
    out <- get(input$egData) ## REAL VERSION (pkg must be compiled)
  } # end eg
  
  #################
  ## .Rdata (df) ##
  #################
  if(input$datatype=="file" && !is.null(input$fileData)){
    out <- NULL
    ## check extension
    oldName <- input$fileData$datapath
    extension <- .readExt(input$fileData$name)
    if(!extension %in% c("RData","Rdata","Rda","RDA", "rdata", "rda")){
      warning("Provided file is not an .RData file")
      print("Provided file is not an .RData file; accepted file extensions: .RData, .Rdata, .Rda, .RDA, .rdata, .rda.")
      return(NULL)
    }
    
    ## need to rename input file
    newName <- paste(input$fileData$datapath, extension, sep=".")
    file.rename(oldName, newName)
    out <- get(load(newName))
    if(class(out)!="data.frame"){
      warning("provided file is not of class 'data.frame'")
      print("Provided file is not of class 'data.frame'.")
      return(NULL)
    }
  } # end file (.Rdata)
  
  #################
  ## EXCEL (csv) ##
  #################
  if(input$datatype=="csv" && !is.null(input$csvData)){
    ## get extension
    oldName <- input$csvData$datapath
    extension <- .readExt(input$csvData$name)
    if(!extension %in% c("csv", "CSV")){
      warning("Provided file is not a .csv file")
      print("Provided file is not a CSV; accepted file extensions: .csv, .xls, .xlsx.")
      return(NULL)
    }
    
    ## need to rename input file
    newName <- paste(input$csvData$datapath, extension, sep=".")
    file.rename(oldName, newName)

    # require("data.table")
    dat <- fread(newName)

    
    ## remove columns containing only NAs
    out <- .noNAcols(out) 

  } # end csv
  
        
  ## RETURN INPUT FILE ##
  
  return(out)
  print(out)
} # end .read.data.input







