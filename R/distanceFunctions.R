
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


############# Mahalanobis distance #############################################

#' Mahalanobis
#'
#' Calculates the Mahalanobis distance for each row (locus, SNP) in the data frame. The input
#' data frame can handle some missing data, as long as a covariance matrix can still be computed from
#' the selected variables.
#'
#' @param dfv a data frame containing observations in rows and statistics in columns
#' @param column.nums indexes the columns of the data frame that contain the statistics used to
#' calculate Mahalanobis distance (all other columns are ignored)
#'
#' @author KE Lotterhos \email{k.lotterhos@neu.edu}
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

Mahalanobis <- function(dfv, column.nums=1:ncol(dfv)){

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

  # calculate variance-covariance matrix. Ignore NA values where possible, but if any NA values appear in final covariance matrix then error
  S <- cov(df.vars, use="pairwise.complete.obs")
  if (any(is.na(S)))
    stop("unable to calculate covariance matrix due to NA values in dfv")

  # check that inverse matrix of S can be calculated (not true if, for example, all values are the same)
  if (class(try(solve(S),silent=TRUE))=='try-error')
    stop("covariance matrix of selected values is exactly singular")

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
#' Values are first standardized in each dimension by subtracting from the
#' mean and dividing by the standard deviation before calculating harmonic mean distances.
#'
#' Note that dfv cannot contain any NA values.
#'
#' @param dfv a data frame containing observations in rows and statistics in columns
#' @param column.nums indexes the columns of the data frame that contain the statistics used to
#' calculate Mahalanobis distance (all other columns are ignored)
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

harmonicDist <- function(dfv, column.nums=1:ncol(dfv)){

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

  # check that no NA values
  if (any(is.na(df.vars)))
    stop("dfv cannot contain NA values")

  # check that at least two rows in df.vars
  nlocs <- nrow(df.vars)
  if (nlocs<2)
    stop("dfv must contain at least two rows")

  #### standardise in each dimension
  n <- nrow(df.vars)
  dim <- ncol(df.vars)
  for (i in 1:dim) {
    df.vars[,i] <- (df.vars[,i]-mean(df.vars[,i]))/sd(df.vars[,i])
  }

  #### calculate harmonic mean distances using C++ function
  distances <- C_harmonicDist(split(t(df.vars),1:dim))$distance

  return(distances)
} # end harmonicDist


############# kernel density distance #############################################

#' Kernel Density Distance
#'
#' Calculates kernel density of all points from all others in multivariate space. Returns -2*log(density)
#' as a distance measure.
#'
#' Takes a matrix or data frame as input, with observations in rows and statistics in columns.
#' Values are first standardized in each dimension by subtracting from the
#' mean and dividing by the standard deviation before calculating kernel density distance. Assumes a
#' multivariate normal kernel with the same user-defined bandwidth in all dimensions (but note that
#' the bandwidth is applied after normalization).
#'
#' Note that dfv cannot contain any NA values.
#'
#' @param dfv a data frame containing observations in rows and statistics in columns
#' @param column.nums indexes the columns of the data frame that contain the statistics used to
#' calculate Mahalanobis distance (all other columns are ignored)
#' @param bandwidth standard deviation of the normal kernel in each dimension
#'
#' @author Robert Verity \email{r.verity@imperial.ac.uk}
#' @examples
#'
#' # create a data frame of observations
#' df <- data.frame(x=rnorm(100),y=rnorm(100))
#'
#' # calculate kernel density distances
#' distances <- harmonicDist(df)
#'
#' # use this distance to look for outliers
#' Q95 <- quantile(distances, 0.95)
#' which(distances>Q95)
#'
#' @export


########################################################################

kernelDist <- function(dfv, column.nums=1:ncol(dfv), bandwidth="default"){

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
  dims <- ncol(df.vars)

  # check that all selected columns are numeric
  if (any(!apply(df.vars,2,is.numeric)))
    stop("all selected columns of dfv must be numeric")

  # check that no NA values
  if (any(is.na(df.vars)))
    stop("dfv cannot contain NA values")

  # check that at least two rows in df.vars
  if (n<2)
    stop("dfv must contain at least two rows")

  # check that bandwidth is either "default" or numeric. If default then apply Silverman's rule.
  if (is.numeric(bandwidth)) {
    if (bandwidth<=0 | !is.finite(bandwidth))
      stop("bandwidth must be greater than 0 and less than infinity")
  } else {
    if (is.na(bandwidth=="default")) {
      stop("bandwidth must be 'default' or numeric")
    } else {
      if (bandwidth=="default") {
        bandwidth = (4/(dims+2))^(1/(dims+4))*n^(-1/(dims+4))
      } else {
        stop("bandwidth must be 'default' or numeric")
      }
    }
  }

  #### standardise in each dimension
  for (i in 1:dims) {
    df.vars[,i] <- (df.vars[,i]-mean(df.vars[,i]))/sd(df.vars[,i])
  }

  #### calculate kernel density distances using C++ function
  distances <- C_kernelDist(split(t(df.vars), 1:dims), bandwidth^2)$distance

  return(distances)
} # end kernelDist


############# kernel density log-likelihood #############################################

#' Kernel Density Log-likelihood
#'
#' Calculates log-likelihood of data under kernel density model for a range of bandwidths.
#' Can be used to estimate the optimal (maximum likelihood) bandwith to use in functions
#' such as kernelDist().
#'
#' Uses same input and model structure as kernelDist(). Calculates the log-likelihood using the
#' leave-one-out method, wherein the likelihood of each point is equal to its kernel
#' density calculated from every *other* point. This avoids the issue of obtaining infinite likelihood
#' at zero bandwidth, which is the case under an ordinary kernel density model.
#'
#'
#' @param dfv a data frame containing observations in rows and statistics in columns
#' @param column.nums indexes the columns of the data frame that contain the statistics used to
#' calculate Mahalanobis distance (all other columns are ignored)
#' @param bandwidth a vector containing the range of bandwidths to be explored
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
#' # obtain log-likelihood at each of these bandwidths
#' logLike <- kernelLogLike(df,bandwidth=lambda)
#'
#' # find the maximum-likelihood bandwidth
#' lambda_ML <- lambda[which.max(logLike)]
#'
#' # use this value when calculating kernel density distances
#' distances <- kernelDist(df,bandwidth=lambda_ML)
#'
#' @export


########################################################################

kernelLogLike <- function(dfv, column.nums=1:ncol(dfv), bandwidth=1:5, reportProgress=FALSE){

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
  dims <- ncol(df.vars)

  # check that all selected columns are numeric
  if (any(!apply(df.vars,2,is.numeric)))
    stop("all selected columns of dfv must be numeric")

  # check that no NA values
  if (any(is.na(df.vars)))
    stop("dfv cannot contain NA values")

  # check that at least two rows in df.vars
  if (n<2)
    stop("dfv must contain at least two rows")

  # check that all elements of bandwidth are numeric and between 0 and infinity
  bandwidth <- as.vector(unlist(bandwidth))
  if (any(!is.numeric(bandwidth)))
    stop("bandwidth must be a numeric vector")
  if (any(bandwidth<=0) | any(!is.finite(bandwidth)))
    stop("bandwidth must contain values greater than 0 and less than infinity")

  #### standardise in each dimension
  for (i in 1:dims) {
    df.vars[,i] <- (df.vars[,i]-mean(df.vars[,i]))/sd(df.vars[,i])
  }

  #### calculate log-likelihood for all bandwidths
  output <- rep(NA,length(bandwidth))
  for (i in 1:length(bandwidth)) {
    if (reportProgress)
      message(paste("bandwidth ",i," of ",length(bandwidth),sep=""))

    output[i] <- C_kernelLogLike(split(t(df.vars), 1:dims), bandwidth[i]^2)$logLike
  }

  return(output)
} # end kernelLogLike

