
########################
## DISTANCE FUNCTIONS ##
########################

# Contains a number of functions that can be used to compute distances, which in turn can be used to
# find outliers. Each of these functions has the same input and output format. Any additional arguments
# to functions should have default values.

# functions:
# Mahalanobis
# harmonicDist
# kernelDist
# kernelLogLike
# neighborDist


############# Mahalanobis distance #############################################

#' Mahalanobis
#'
#' Calculates the Mahalanobis distance for each row (locus, SNP) in the data frame. The input
#' data frame can handle some missing data, as long as a covariance matrix can still be computed from
#' the selected variables.
#'
#' @param dfv a data frame containing observations in rows and statistics in columns.
#' @param column.nums indexes the columns of the data frame that contain the statistics used to
#' calculate Mahalanobis distance (all other columns are ignored).
#' @param S the covariance matrix used to normalise the data in the Mahalanobis calculation. Leave as NULL to use the ordinary covariance matrix calculated on the data.
#'
#' @author Robert Verity \email{r.verity@imperial.ac.uk}
#' @examples
#'
#' # create a matrix of observations
#' df <- data.frame(x=rnorm(100),y=rnorm(100))
#'
#' # calculate Mahalanobis distances
#' distances <- Mahalanobis(df)
#'
#' # use this distance to look for outliers
#' Q95 <- quantile(distances, 0.95)
#' which(distances>Q95)
#'
#' @export

########################################################################

Mahalanobis <- function(dfv, column.nums=1:ncol(dfv), S=NULL){

  #### perform simple checks on data
  # check that dfv is a matrix or data frame
  if (!is.matrix(dfv) & !is.data.frame(dfv))
    stop("dfv must be a matrix or data frame")

  # check that column.nums can be used to index dfv without error
  if (class(try(dfv[,column.nums],silent=TRUE))=='try-error')
    stop("column.nums must contain valid indexes for choosing columns in dfv")

  # extract variables from dfv
  df.vars <- as.matrix(dfv[,column.nums,drop=FALSE])

  # check that all selected columns are numeric
  if (any(!apply(df.vars,2,is.numeric)))
    stop("all selected columns of dfv must be numeric")

  # check that at least two rows in df.vars
  nlocs <- nrow(df.vars)
  if (nlocs<2)
  	stop("dfv must contain at least two rows")

  # if S is NULL replace with covariance matrix
  if (is.null(S)) 
    S <- cov(df.vars, use="pairwise.complete.obs")
  
  # check that S is a matrix
  if (!is.matrix(S))
    stop("S must be a matrix")
  
  # check that S has the same number of rows and cols as variables in df.vars
  if (nrow(S)!=ncol(df.vars) | ncol(S)!=ncol(df.vars))
    stop("S must contain the same number of rows and columns as there are selected variables in dfv")

  # check that S contains no NA values
  if (any(is.na(S)))
    stop("covariance matrix S contains NA values")

  # check that inverse matrix of S can be calculated (not true if, for example, all values are the same)
  if (class(try(solve(S),silent=TRUE))=='try-error')
    stop("covariance matrix S is exactly singular")

  # calculate Mahalanobis distance
  diff <- df.vars
  for (i in 1:ncol(df.vars)) {
    diff[,i] <- diff[,i] - mean(diff[,i], na.rm=TRUE)
  }
  distance <- sqrt( rowSums((diff %*% solve(S))*diff) )

  return(distance)
} # end Mahalanobis


############# harmonic mean distance #############################################

#' Harmonic Mean Distance
#'
#' Calculates harmonic mean
#' distance of all points from all others.
#'
#' Takes a matrix or data frame as input, with observations in rows and statistics in columns.
#' Values are first normalized in each dimension by subtracting from the
#' mean and multiplying by the inverse covariance matrix before calculating harmonic mean distances.
#'
#' Note that this method cannot handle NA values.
#'
#' @param dfv a data frame containing observations in rows and statistics in columns.
#' @param column.nums indexes the columns of the data frame that contain the statistics used to
#' calculate harmonic distance (all other columns are ignored).
#' @param S the covariance matrix used to normalise the data in the harmonic mean calculation. Leave as NULL to use the ordinary covariance matrix calculated on the data.
#'
#' @author Robert Verity \email{r.verity@imperial.ac.uk}
#' @examples
#'
#' # create a data frame of observations
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

harmonicDist <- function(dfv, column.nums=1:ncol(dfv), S=NULL){
    
    #### perform simple checks on data
    # check that dfv is a matrix or data frame
    if (!is.matrix(dfv) & !is.data.frame(dfv))
    stop("dfv must be a matrix or data frame")
    
    # check that column.nums can be used to index dfv without error
    if (class(try(dfv[,column.nums],silent=TRUE))=='try-error')
    stop("column.nums must contain valid indexes for choosing columns in dfv")
    
    # extract variables from dfv
    df.vars <- as.matrix(dfv[,column.nums,drop=FALSE])
    n <- nrow(df.vars)
    d <- ncol(df.vars)
    
    # check that all selected columns are numeric
    if (any(!apply(df.vars,2,is.numeric)))
    stop("all selected columns of dfv must be numeric")
    
    # check that no NA values
    if (any(is.na(df.vars)))
    stop("dfv cannot contain NA values")
    
    # check that at least two rows in df.vars
    nlocs <- nrow(df.vars)
    if (nlocs<2)
    stop("dfv must contain at least two rows")
    
    # if S is NULL replace with covariance matrix
    if (is.null(S)) 
      S <- cov(df.vars)
    
    # check that S is a matrix
    if (!is.matrix(S))
      stop("S must be a matrix")
    
    # check that S has the same number of rows and cols as variables in df.vars
    if (nrow(S)!=ncol(df.vars) | ncol(S)!=ncol(df.vars))
      stop('S must contain the same number of rows and columns as there are selected variables in dfv')
    
    # check that S contains no NA values
    if (any(is.na(S)))
      stop("covariance matrix S contains NA values")
    
    # check that inverse matrix of S can be calculated (not true if, for example, all values are the same)
    if (class(try(solve(S),silent=TRUE))=='try-error')
      stop("covariance matrix S is exactly singular")
    
    # calculate inverse covariance matrix
    S_inv <- solve(S)
    
    #### calculate harmonic mean distances using C++ function
    distances <- C_harmonicDist(split(t(df.vars),1:d), split(S_inv,1:d))$distance
    
    return(distances)
} # end harmonicDist


############# nearest neighbor distance #############################################

#' Nearest Neighbor Distance
#'
#' Calculates euclidean distance to nearest neighbor for all points.
#'
#' Takes a matrix or data frame as input, with observations in rows and statistics in columns.
#' Values are first normalized in each dimension by subtracting from the
#' mean and dividing by the standard deviation before calculating nearest neighbor distance.
#'
#' Note that this method cannot handle NA values.
#'
#' @param dfv a data frame containing observations in rows and statistics in columns.
#' @param column.nums indexes the columns of the data frame that contain the statistics used to
#' calculate nearest neighbor distance (all other columns are ignored).
#' @param S the covariance matrix used to normalise the data in the nearest neighbor calculation. Leave as NULL to use the ordinary covariance matrix calculated on the data.
#'
#' @author Robert Verity \email{r.verity@imperial.ac.uk}
#' @examples
#'
#' # create a data frame of observations
#' df <- data.frame(x=rnorm(100),y=rnorm(100))
#'
#' # calculate nearest neighbor distances
#' distances <- neighborDist(df)
#'
#' # use this distance to look for outliers
#' Q95 <- quantile(distances, 0.95)
#' which(distances>Q95)
#'
#' @export


########################################################################

neighborDist <- function(dfv, column.nums=1:ncol(dfv), S=NULL){
  
  #### perform simple checks on data
  # check that dfv is a matrix or data frame
  if (!is.matrix(dfv) & !is.data.frame(dfv))
    stop("dfv must be a matrix or data frame")
  
  # check that column.nums can be used to index dfv without error
  if (class(try(dfv[,column.nums],silent=TRUE))=='try-error')
    stop("column.nums must contain valid indexes for choosing columns in dfv")
  
  # extract variables from dfv
  df.vars <- as.matrix(dfv[,column.nums,drop=FALSE])
  n <- nrow(df.vars)
  d <- ncol(df.vars)
  
  # check that all selected columns are numeric
  if (any(!apply(df.vars,2,is.numeric)))
    stop("all selected columns of dfv must be numeric")
  
  # check that no NA values
  if (any(is.na(df.vars)))
    stop("dfv cannot contain NA values")
  
  # check that at least two rows in df.vars
  if (n<2)
    stop("dfv must contain at least two rows")
  
  # if S is NULL replace with covariance matrix
  if (is.null(S)) 
    S <- cov(df.vars)
  
  # check that S is a matrix
  if (!is.matrix(S))
    stop("S must be a matrix")
  
  # check that S has the same number of rows and cols as variables in df.vars
  if (nrow(S)!=ncol(df.vars) | ncol(S)!=ncol(df.vars))
    stop('S must contain the same number of rows and columns as there are selected variables in dfv')
  
  # check that S contains no NA values
  if (any(is.na(S)))
    stop("covariance matrix S contains NA values")
  
  # check that inverse matrix of S can be calculated (not true if, for example, all values are the same)
  if (class(try(solve(S),silent=TRUE))=='try-error')
    stop("covariance matrix S is exactly singular")
  
  # calculate inverse covariance matrix
  S_inv <- solve(S)
  
  #### calculate nearest neighbor distances using C++ function
  distances <- C_neighborDist(split(t(df.vars), 1:d), split(S_inv,1:d))$distance
  
  return(distances)
} # end neighborDist

############# kernel density distance #############################################

#' Kernel Density Distance
#'
#' Calculates kernel density of all points from all others in multivariate space. Returns -2*log(density)
#' as a distance measure.
#'
#' Takes a matrix or data frame as input, with observations in rows and statistics in columns.
#' Values are first normalized in each dimension by subtracting from the
#' mean and dividing by the standard deviation before calculating kernel density distance. Assumes a
#' multivariate normal kernel with the same user-defined bandwidth in all dimensions (after normalization).
#'
#' Note that this method cannot handle NA values.
#'
#' @param dfv a data frame containing observations in rows and statistics in columns.
#' @param column.nums indexes the columns of the data frame that contain the statistics used to
#' calculate kernel distance (all other columns are ignored).
#' @param bandwidth standard deviation of the normal kernel in each dimension. Can be a numerical value, or can be set to 'default', in which case Silverman's rule is used to select the bandwidth.
#' @param S the covariance matrix that the bandwidth is multiplied by. Leave as NULL to use the ordinary covariance matrix calculated on the data.
#'
#' @author Robert Verity \email{r.verity@imperial.ac.uk}
#' @examples
#'
#' # create a data frame of observations
#' df <- data.frame(x=rnorm(100),y=rnorm(100))
#'
#' # calculate kernel density distances
#' distances <- kernelDist(df)
#'
#' # use this distance to look for outliers
#' Q95 <- quantile(distances, 0.95)
#' which(distances>Q95)
#'
#' @export


########################################################################

kernelDist <- function(dfv, column.nums=1:ncol(dfv), bandwidth="default", S=NULL){
    
    #### perform simple checks on data
    # check that dfv is a matrix or data frame
    if (!is.matrix(dfv) & !is.data.frame(dfv))
    stop("dfv must be a matrix or data frame")
    
    # check that column.nums can be used to index dfv without error
    if (class(try(dfv[,column.nums],silent=TRUE))=='try-error')
    stop("column.nums must contain valid indexes for choosing columns in dfv")
    
    # extract variables from dfv
    df.vars <- as.matrix(dfv[,column.nums,drop=FALSE])
    n <- nrow(df.vars)
    d <- ncol(df.vars)
    
    # check that all selected columns are numeric
    if (any(!apply(df.vars,2,is.numeric)))
    stop("all selected columns of dfv must be numeric")
    
    # check that no NA values
    if (any(is.na(df.vars)))
    stop("dfv cannot contain NA values")
    
    # check that at least two rows in df.vars
    if (n<2)
    stop("dfv must contain at least two rows")
    
    # if S is NULL replace with covariance matrix
    if (is.null(S)) 
      S <- cov(df.vars)
    
    # check that S is a matrix
    if (!is.matrix(S))
      stop("S must be a matrix")
    
    # check that S has the same number of rows and cols as variables in df.vars
    if (nrow(S)!=ncol(df.vars) | ncol(S)!=ncol(df.vars))
      stop('S must contain the same number of rows and columns as there are selected variables in dfv')
    
    # check that S contains no NA values
    if (any(is.na(S)))
      stop("covariance matrix S contains NA values")
    
    # check that inverse matrix of S can be calculated (not true if, for example, all values are the same)
    if (class(try(solve(S),silent=TRUE))=='try-error')
      stop("covariance matrix S is exactly singular")
    
    # calculate inverse covariance matrix
    S_inv <- solve(S)
    
    # check that bandwidth is either "default" or numeric. If default then apply Silverman's rule.
    if (is.numeric(bandwidth)) {
        if (bandwidth<=0 | !is.finite(bandwidth))
        stop("bandwidth must be greater than 0 and less than infinity")
    } else {
        if (is.na(bandwidth=="default")) {
            stop("bandwidth must be 'default' or numeric")
        } else {
            if (bandwidth=="default") {
                bandwidth = (4/(d+2))^(1/(d+4))*n^(-1/(d+4))
            } else {
                stop("bandwidth must be 'default' or numeric")
            }
        }
    }
    
    #### calculate kernel density distances using C++ function
    distances <- C_kernelDist(split(t(df.vars), 1:d), bandwidth^2, split(S_inv,1:d))$distance
    
    return(distances)
} # end kernelDist


############# kernel density deviabce #############################################

#' Kernel Density Deviance
#'
#' Calculates the Bayesian deviance (-2*log-likelihood) under the same kernel density model used
#' by kernelDist() for a range of bandwidths. Can be used to estimate the optimal
#' (maximum likelihood) bandwith to use in the kernelDist() function (see example).
#'
#' Uses same input and model structure as kernelDist(). Calculates the log-likelihood using the
#' leave-one-out method, wherein the likelihood of each point is equal to its kernel
#' density calculated from every *other* point. This avoids the issue of obtaining infinite likelihood
#' at zero bandwidth, which would be the case under an ordinary kernel density model.
#'
#'
#' @param dfv a data frame containing observations in rows and statistics in columns.
#' @param column.nums indexes the columns of the data frame that contain the statistics used to
#' calculate kernel log-likelihood (all other columns are ignored).
#' @param bandwidth a vector containing the range of bandwidths to be explored.
#' @param S the covariance matrix that the bandwidth is multiplied by. Leave as NULL to use the ordinary covariance matrix calculated on the data.
#' @param reportProgress whether to report current progress of the algorithm to the console (TRUE/FALSE).
#'
#' @author Robert Verity \email{r.verity@imperial.ac.uk}
#' @examples
#'
#' # create a data frame of observations
#' df <- data.frame(x=rnorm(100),y=rnorm(100))
#'
#' # create a vector of bandwidths to explore
#' lambda <- seq(0.1,2,0.1)
#'
#' # obtain deviance at each of these bandwidths
#' deviance <- kernelDeviance(df,bandwidth=lambda,reportProgress=TRUE)
#'
#' # find the maximum-likelihood (minimum-deviance) bandwidth
#' lambda_ML <- lambda[which.min(deviance)]
#'
#' # use this value when calculating kernel density distances
#' distances <- kernelDist(df,bandwidth=lambda_ML)
#'
#' @export


########################################################################

kernelDeviance <- function(dfv, column.nums=1:ncol(dfv), bandwidth=seq(0.1,1,0.1), S=NULL, reportProgress=FALSE){
    
    #### perform simple checks on data
    # check that dfv is a matrix or data frame
    if (!is.matrix(dfv) & !is.data.frame(dfv))
    stop("dfv must be a matrix or data frame")
    
    # check that column.nums can be used to index dfv without error
    if (class(try(dfv[,column.nums],silent=TRUE))=='try-error')
    stop("column.nums must contain valid indexes for choosing columns in dfv")
    
    # extract variables from dfv
    df.vars <- as.matrix(dfv[,column.nums,drop=FALSE])
    n <- nrow(df.vars)
    d <- ncol(df.vars)
    
    # check that all selected columns are numeric
    if (any(!apply(df.vars,2,is.numeric)))
    stop("all selected columns of dfv must be numeric")
    
    # check that no NA values
    if (any(is.na(df.vars)))
    stop("dfv cannot contain NA values")
    
    # check that at least two rows in df.vars
    if (n<2)
    stop("dfv must contain at least two rows")
    
    # if S is NULL replace with covariance matrix
    if (is.null(S)) 
      S <- cov(df.vars)
    
    # check that S is a matrix
    if (!is.matrix(S))
      stop("S must be a matrix")
    
    # check that S has the same number of rows and cols as variables in df.vars
    if (nrow(S)!=ncol(df.vars) | ncol(S)!=ncol(df.vars))
      stop('S must contain the same number of rows and columns as there are selected variables in dfv')
    
    # check that S contains no NA values
    if (any(is.na(S)))
      stop("covariance matrix S contains NA values")
    
    # check that inverse matrix of S can be calculated (not true if, for example, all values are the same)
    if (class(try(solve(S),silent=TRUE))=='try-error')
      stop("covariance matrix S is exactly singular")
    
    # calculate inverse covariance matrix
    S_inv <- solve(S)
    
    # check that all elements of bandwidth are numeric and between 0 and infinity
    bandwidth <- as.vector(unlist(bandwidth))
    if (any(!is.numeric(bandwidth)))
      stop("bandwidth must be a numeric vector")
    if (any(bandwidth<=0) | any(!is.finite(bandwidth)))
      stop("bandwidth must contain values greater than 0 and less than infinity")
    
    #### calculate deviance for all bandwidths
    output <- rep(NA,length(bandwidth))
    for (i in 1:length(bandwidth)) {
        if (reportProgress) {
            message(paste("bandwidth ",i," of ",length(bandwidth),sep=""))
            flush.console()
        }
        output[i] <- C_kernelDeviance(split(t(df.vars), 1:d), bandwidth[i]^2, split(S_inv,1:d))$deviance
    }
    
    return(output)
} # end kernelDeviance


