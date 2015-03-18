#'  KE Lotterhos
#'  Aug 11, 2014
#'  Updated March 8, 2015
#'  Empirical p-value Malanahobis distance
#'  @param dfv is a dataframe containing the observations in rows and the statistics in columns
#'  @param column.nums is the column numbers of the dataframe containing the statistics used to calculate Mahalanobis distance
#'  @author KE Lotterhos
#'  @export



Mahalanobis <- function(dfv, column.nums){
# This function calculates the Mahalanobis distance for each row (locus, SNP) in the dataframe
# dfv is a dataframe with each row a locus or population, and columns statistics and other information
# column.nums is the columns in the dataframe to be used for analysis
# haven't tested with missing data
  df.vars <- dfv[,column.nums]
  nlocs <- nrow(df.vars)
  mu <- t(t(colMeans(df.vars, na.rm=TRUE))) # calculate mean and make it a column vector
  S <- cov(df.vars, use="pairwise.complete.obs") # calculate variance-covariance matrix
  
  D <- rep(NA, nlocs) # Mahalanobis distance
  for (i in 1:nrow(df.vars)){
	  if(sum(is.na(df.vars[i,]))==0){  # The locus has to have an observation for each test statistic
    x <- t(df.vars[i,])
    diff <- x-mu
    D[i] <- sqrt( t(diff) %*% solve(S) %*% (diff) )
	  }else{ # or else NA
		  D[i]=NA
	  }
  }
  return(D)
} # end Mahalanobis

