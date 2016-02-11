
############# harmonic mean distance #############################################

#' Harmonic Mean Distance
#'
#' Calculates harmonic mean
#' distance of all points from all others.
#'
#' Takes a matrix or data frame as input, with observations in rows and statistics in columns.
#' Values are first standardized in each dimension by subtracting from the
#' mean and dividing by the standard deviation before calculating harmonic mean distances.
#'
#' Note that dfv cannot contain any NA values.
#'
#' @param dfv is a matrix or data frame
#' containing observations in rows and statistics in columns.
#'
#' @author Robert Verity \email{r.verity@imperial.ac.uk}
#' @examples
#'
#' # create a matrix of observations
#' df <- data.frame(x=rnorm(100),y=rnorm(100))
#'
#' # calculate harmonic mean distances
#' distances <- harmonicDist(df)
#'
#' # use this distance to look for outliers
#' Q95 <- quantile(distances, 0.95)
#' which(distances>Q95)
#'
#' @export

########################################################################

harmonicDist <- function(dfv){

    #### perform simple checks on data
    # check that dfv is a matrix or data frame
    if (!is.matrix(dfv) & !is.data.frame(dfv))
        stop("dfv must be a matrix or data frame (even for one-dimensional data)")

    # check that numeric
    if (any(!apply(dfv,2,is.numeric)))
        stop("dfv must be numeric")

    # check that no NA values
    if (any(is.na(dfv)))
        stop("dfv cannot contain NA values")

    #### standardise in each dimension
    n <- nrow(dfv)
    dim <- ncol(dfv)
    for (i in 1:dim) {
    	dfv[,i] <- (dfv[,i]-mean(dfv[,i]))/sd(dfv[,i])
    }

    #### calculate harmonic mean distances using C++ function
    distances <- C_harmonicDist(split(t(dfv),1:dim))$harmonic

    return(distances)

} # end harmonicDist
