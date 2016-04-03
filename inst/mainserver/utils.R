

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


#########   ###   ###   ###   ###   ###   ###   ###   ###   ###   #########

#####################
## .is.wholenumber ##
#####################

########################################################################

###################
## DOCUMENTATION ##
###################

#' Test if number is a whole number.
#'
#' Function to test if a number of vector of numbers has no decimal places.
#' Note that is.integer just tests class.
#'
#' @param x A numeric value or vector of numeric values.
#' @param tol A function or number specifying the smallest positive value the machine being used can recognise.
#'
#' @author Caitlin Collins \email{caitiecollins@@gmail.com}
#' @export

########################################################################

.is.wholenumber <- function(x, tol = .Machine$double.eps^0.5){
  abs(x - round(x)) < tol
}  # end .is.wholenumber

###############
## keepLastN ##
###############

########################################################################

###################
## DOCUMENTATION ##
###################

#' Truncate to keep only the \emph{last} N characters.
#'
#' Truncate an element, or each element of a vector, by
#' removing all but the last N characters of each element.
#'
#' @param x A vector whose element(s) will be truncated.
#' @param n An integer specifying the number of characters to \emph{keep}.
#'
#' @author Caitlin Collins \email{caitiecollins@@gmail.com}
#' @export

########################################################################

.keepLastN <- function(x, n){
  sapply(x, function(e)
    substr(e, (nchar(e)-n+1), nchar(e))
  )
} # end .keepLastN

################
## keepFirstN ##
################

########################################################################

###################
## DOCUMENTATION ##
###################

#' Truncate to keep only the \emph{first} N characters.
#'
#' Truncate an element, or each element of a vector, by
#' removing all but the first N characters of each element.
#'
#' @param x A vector whose element(s) will be truncated.
#' @param n An integer specifying the number of characters to \emph{keep}.
#'
#' @author Caitlin Collins \email{caitiecollins@@gmail.com}
#' @export

########################################################################

.keepFirstN <- function(x, n){
  sapply(x, function(e)
    substr(e, (nchar(e)-n+1), nchar(e))
  )
} # end .keepFirstN

#################
## removeLastN ##
#################

########################################################################

###################
## DOCUMENTATION ##
###################

#' Truncate to remove all of the \emph{last} N characters.
#'
#' Truncate an element, or each element of a vector, by
#' removing the last N characters of each element.
#'
#' @param x A vector whose element(s) will be truncated.
#' @param n An integer specifying the number of characters to \emph{remove}.
#'
#' @author Caitlin Collins \email{caitiecollins@@gmail.com}
#' @export

########################################################################

.removeLastN <- function(x, n){
  sapply(x, function(e)
    substr(e, 0, (nchar(e)-n))
  )
} # end .removeLastN


##################
## removeFirstN ##
##################

########################################################################

###################
## DOCUMENTATION ##
###################

#' Truncate to remove all of the \emph{first} N characters.
#'
#' Truncate an element, or each element of a vector, by
#' removing the first N characters of each element.
#'
#' @param x A vector whose element(s) will be truncated.
#' @param n An integer specifying the number of characters to \emph{remove}.
#'
#' @author Caitlin Collins \email{caitiecollins@@gmail.com}
#' @export

########################################################################

.removeFirstN <- function(x, n){
  sapply(x, function(e)
    substr(e, n+1, nchar(e))
  )
} # end .removeFirstN


#########   ###   ###   ###   ###   ###   ###   ###   ###   ###   #########

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
###############
## .noNAcols ##
###############

.noNAcols <- function(x) {
  NAsByColumn <- sapply(c(1:ncol(x)), function(e)
    if(class(x[,e])=="Date"){ # if the column is a date column containing something other than blanks
      which(is.na(x[,e]))
    }else{
      c(which(x[,e]==""), which(x[,e]=="NR"), which(is.na(x[,e])))
    }
  )
  colsToRemove <- which(sapply(c(1:length(NAsByColumn)), function(e) length(NAsByColumn[[e]]))==dim(x)[[1]])
  if(!.is.integer0(colsToRemove)){
    x <- x[,-colsToRemove] ## remove columns that are all NA
    NAsByColumn <- NAsByColumn[-colsToRemove]
  }
  for(j in 1:length(NAsByColumn)){ ## replace "" and NR with NA
    if(!.is.integer0(NAsByColumn[[j]])){
      x[,j] <- replace(x[,j], NAsByColumn[[j]], NA)
    }
  }
  return(x)
} # end .noNAcols


###########################################################################################################################################################

##############
## .showNAs ##
##############

## Mini fn that takes a data.frame as input
## and returns a data.frame for which
## NA values in the input df will be
## shown in a shiny renderDataTable output.

.showNAs <- function(df){
  mat <- as.matrix(df)
  mat[is.na(mat)] <- c("NA")
  df <- as.data.frame(mat)
  return(df)
} # end .showNAs


###########################################################################################################################################################

