

###################
## UI FUNCTIONS! ##
###################

## .getxSelection ##
.getxSelection <- function(mainData){
  out <- NULL
  if(!is.null(mainData)){
    
    sel <- NULL
    ## get numeric variables
    numCols <- which(sapply(c(1:ncol(mainData)), function(e) is.numeric(mainData[,e])))
    if(length(numCols) > 0){
      sel <- names(mainData)[numCols[1]]
    }
    
    out <- selectInput(
      inputId = "xSelection",
      label = "Choose x-axis data:",
      choices = names(mainData),
      selected = sel #may want to update with publishable dataset
    )
  }
  return(out)
} # end .getxSelection


## .getySelection ##
.getySelection <- function(mainData){
  out <- NULL
  if(!is.null(mainData)){
    sel <- NULL
    ## get numeric variables
    numCols <- which(sapply(c(1:ncol(mainData)), function(e) is.numeric(mainData[,e])))
    if(length(numCols) > 0){
      sel <- names(mainData)[numCols[1]]
      if(length(numCols) >= 2){
        sel <- names(mainData)[numCols[2]]
      }
    }
    
    out <- selectInput(
      inputId = "ySelection",
      label = "Choose y-axis data:",
      choices = names(mainData),
      selected = sel #may want to update with publishable dataset
    )
  }
  return(out)
} # end .getySelection

.getpSelection <- function(mainData){
  out <- NULL
  if(!is.null(mainData)){
    out <- selectInput(
      inputId = "pSelection",
      label = "Choose p value cutoff:",
      choices = names(mainData)
    )
  }
  return(out)
} # end .getxSelection


## .getColVarSelection ##
.getColVarSelection <- function(mainData){
  out <- NULL
  if(!is.null(mainData)){
    
    sel <- NULL
    ## get numeric variables
    numCols <- which(sapply(c(1:ncol(mainData)), function(e) is.numeric(mainData[,e])))
    if(length(numCols) > 0){
      sel <- names(mainData)[numCols[1]]
      if(length(numCols) >= 3){
        sel <- names(mainData)[numCols[3]]
      }
    }
    
    out <- selectInput(
      inputId = "colVarSelection",
      label = "Choose variable to highlight Outliers for on plot:",
      choices = names(mainData),
      selected = sel #may want to update with publishable dataset
    )
  }  
  return(out)
} # end .getColVarSelection


## .getColPal ##
.getColPal <- function(){
  out <- selectInput("colPal", "Choose a color pallette to use:",
                     c("SeaSun" = "seasun",
                       "Funky" = "funky",
                       "Spectral" = "spectral",
                       "Azur" = "azur",
                       "Wasp" = "wasp"
                     )
  )
  return(out)
} # end .getColPal

