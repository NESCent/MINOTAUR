

###################
## UI FUNCTIONS! ##
###################

## .getxSelection ##
.getxSelection <- function(mainData){
  out <- NULL
  if(!is.null(mainData)){
    out <- selectInput(
      inputId = "xSelection",
      label = "Choose x-axis data:",
      choices = names(mainData),
      selected = names(mainData)[4] #will have to update with publishable dataset
    )
  }
  return(out)
} # end .getxSelection


## .getySelection ##
.getySelection <- function(mainData){
  out <- NULL
  if(!is.null(mainData)){
    out <- selectInput(
      inputId = "ySelection",
      label = "Choose y-axis data:",
      choices = names(mainData),
      selected = names(mainData)[5] #will have to update with publishable dataset
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
    out <- selectInput(
      inputId = "colVarSelection",
      label = "Choose variable to overlay points by (not working):",
      choices = names(mainData),
      selected = names(mainData)[5] #will have to update with publishable dataset
    )
  }  
  return(out)
} # end .getColVarSelection


## .getColPal ##
.getColPal <- function(){
  out <- selectInput("colPal", "Choose a color pallette to use:",
    c("Just black" = "black",
      "SeaSun" = "seasun",
      "Funky" = "funky",
      "Spectral" = "spectral",
      "Azur" = "azur",
      "Wasp" = "wasp"
    )
  )
  return(out)
} # end .getColPal

