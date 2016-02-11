
############# Mahalanobis distance #############################################

# NOTE
# still need to standardize inputs and particularly outputs across different outlier functions

#' Mahalanobis
#'
#' Calculates the Mahalanobis distance for each row (locus, SNP) in the data frame. The input data frame can handle some missing data, as long as a covariance matrix can still be computed.
#'  @param dfv is a data frame containing observations in rows and statistics in columns
#'  @param column.nums is the column numbers of the data frame containing the statistics used to calculate Mahalanobis distance (other columns are ignored)
#'
#' @author KE Lotterhos \email{k.lotterhos@neu.edu}
#' @examples
#'
#' # (STILL NEEDS DOING)
#'
#' @export

########################################################################

Mahalanobis <- function(dfv, column.nums=1:ncol(dfv)){
  
  # check that dfv is a two-dimensional object such as a matrix or data frame
  if (is.null(dim(dfv))) 
  	stop("dfv must be a two-dimensional object such as a matrix or data frame")
  
  # check that at least two rows
  nlocs <- nrow(dfv)
  if (nlocs<2)
  	stop("dfv must contain at least two rows")
  
  # check that all selected columns contain numeric values
  if (any(!mapply(is.numeric,dfv[,column.nums])))
  	stop("all selected columns of dfv must be numeric")
  
  # extract matrix of values
  df.vars <- mapply(as.numeric,dfv[,column.nums])
  
  # calculate mean of each variable
  mu <- colMeans(df.vars, na.rm=TRUE)
  
  # calculate variance-covariance matrix. Ignore NA values where possible, but if any NA values appear in final matrix then error
  S <- cov(df.vars, use="pairwise.complete.obs") 
  if (any(is.na(S))) {stop("unable to calculate covariance matrix due to NA values in dfv")}
  
  # calculate Mahalanobis distance
  diff <- df.vars
  for (i in 1:ncol(df.vars)) {
    diff[,i] <- diff[,i]  - mu[i]
  }
  D <- sqrt( rowSums((diff %*% solve(S))*diff) )
  
  # rank D values and return along with -log(p), where p is empirical p-value
  Dm.rank <- nlocs-rank(D)+1
  minus.log.emp.p <- -log(Dm.rank/nlocs)
  
  return(list(Dm=D, Dm.rank=Dm.rank, minus.log.emp.p=minus.log.emp.p))
} # end Mahalanobis
