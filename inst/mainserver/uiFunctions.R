

###################
## UI FUNCTIONS! ##
###################

## .getxSelection ##
.getxSelection <- function(mainData){
  out <- NULL
  if(!is.null(mainData)){
    out <- selectInput(inputId = "xSelection",
                       label = "Choose x-axis data:",
                       choices = names(mainData),
                       selected = names(mainData)[1])
  }
  return(out)
} # end .getxSelection


## .getySelection ##
.getySelection <- function(mainData){
  out <- NULL
  if(!is.null(mainData)){
    out <- selectInput(inputId = "ySelection",
                label = "Choose y-axis data:",
                choices = names(mainData),
                selected = names(mainData)[2])
  }
  return(out)
} # end .getySelection


## .getColPal ##
.getColPal <- function(){
  out <- selectInput("col.pal", "Choose a color pallette to use:",
                     c("SeaSun" = "seasun",
                       "Funky" = "funky",
                       "Spectral" = "spectral",
                       "Azur" = "azur",
                       "Wasp" = "wasp"
                     )
  )
  return(out)
} # end .getColPal

