

###########
## UTILS ##
###########


## NOTE: ##
## THIS FILE IS WHERE YOU SHOULD PUT ANY 
## GENERIC, USEFUL FUNCTIONS,
## Eg. If Your function is/can be used in the code of 
## MORE THAN ONE of the other .R files, put it here!

## NOTE 2: ##
## UNLESS YOU WANT TO DOCUMENT YOUR FUNCTION
## AND SHARE IT WITH USERS AS A STAND-ALONE FUNCTION, 
## THEN YOU SHOULD CALL IT ".fn" (ie. put a dot at the
## start of its name, here and everywhere you use it)




###########################################################################################################################################################

##################
## .is.integer0 ##
##################

## fn testing for output==integer(0)
.is.integer0 <- function(x){
  is.integer(x) && length(x) == 0L
} # end .is.integer0

###########################################################################################################################################################

################
## .is.letter ##
################

## fn testing whether the contents of a vector are letters regardless of their class
## NOTE: interpret TRUE as "contains letters" (ie. will return TRUE for BOTH "aa" AND "a1")
.is.letter <- function(x) grepl("[[:alpha:]]", x)

###########################################################################################################################################################

################
## .is.number ##
################

## fn testing whether the contents of a vector are numbers regardless of their class
## NOTE: interpret TRUE as "contains number" (ie. will return TRUE for BOTH "11" AND "1a")
.is.number <- function(x) grepl("[[:digit:]]", x)

###########################################################################################################################################################

##################
## .substrRight ##
##################

# truncate character string from right
.substrRight <- function(x, n){
  sapply(x, function(xx)
    substr(xx, (nchar(xx)-n+1), nchar(xx))
  )
} # end .substrRight

###########################################################################################################################################################

#################
## .substrLeft ##
#################

# truncate character string from left
.substrLeft <- function(x, n){
  sapply(x, function(xx)
    substr(xx, 0, n)
  )
} # end .substrLeft

###########################################################################################################################################################

####################
## .decimalplaces ##
####################

.decimalplaces <- function(x) {
  if(is.na(x)){
    return(0)
  }else{
    if ((x %% 1) != 0) {
      nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
    } else {
      return(0)
    }
  }
} # .decimalplaces

###########################################################################################################################################################

################
## .is.letter ##
################

## fn testing whether the contents of a vector are letters regardless of their class
## NOTE: interpret TRUE as "contains letters" (ie. will return TRUE for BOTH "aa" AND "a1")
.is.letter <- function(x) grepl("[[:alpha:]]", x)


###########################################################################################################################################################

################
## .is.number ##
################

## fn testing whether the contents of a vector are numbers regardless of their class
## NOTE: interpret TRUE as "contains number" (ie. will return TRUE for BOTH "11" AND "1a")
.is.number <- function(x) grepl("[[:digit:]]", x)


###########################################################################################################################################################

