

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
      choices = names(mainData)
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
      selected = names(mainData)[2]
    )
  }
  return(out)
} # end .getySelection


## .getColVarSelection ##
.getColVarSelection <- function(mainData){
  out <- NULL
  if(!is.null(mainData)){
    out <- selectInput(
      inputId = "colVarSelection",
      label = "Choose variable to color by:",
      choices = names(mainData),
      selected = names(mainData)[1]
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

