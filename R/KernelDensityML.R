#'  R Verity
#'  March 18, 2015
#'  Kernel density with maximum likelihood bandwidth selection
#'  @name KernelDensityML
#'  @param dfv is a dataframe containing the observations in rows and the statistics in columns
#'  @param column.nums is the column numbers of the dataframe containing the statistics used to calculate kernel density distance
#'  @author R Verity
#'  @export KernelDensityML
#'  @examples
#'  ##TO DO


## .logLike_i ##
# calculate density of point i from all other points
.logLike_i = function(df,i,lambda) {
  rawProbs = rowSums(apply(df,2,function(x){dnorm(x[i],x[-i],sd=lambda,log=TRUE)}))
  totalProb = log(mean(exp(rawProbs)))
  return(totalProb)
} # end .logLike_i

## .deviance_all ##
# calculate deviance (-2*logLikelihood) of all points in data frame
.deviance_all = function(df,lambda) {
  -2*sum(mapply(.logLike_i,i=1:nrow(df),MoreArgs=list(df=df,lambda=lambda)))
} # end .deviance_all

X = data.frame(t1=rnorm(100),t2=rnorm(100))

.deviance_all(X,0.1)


## KernelDensityML ##
KernelDensityML <- function(dfv, column.nums){
  # This function produces a kernel density from the selected points, and returns the kernel 'distance'
  # as -2*logLikelihood of all points under this model. Kernel bandwidth is chosen automatically by
  # maximum likelihood.
  # Nb. In theory can work with missing data, but not coded up yet
  
  # subset data
  df.vars <- dfv[,column.nums]
  nlocs <- nrow(df.vars)
  
  
  
#   D <- rep(NA, nlocs) # Mahalanobis distance
#   for (i in 1:nrow(df.vars)){
#     if(sum(is.na(df.vars[i,]))==0){  # The locus has to have an observation for each test statistic
#       x <- t(df.vars[i,])
#       diff <- x-mu
#       D[i] <- sqrt( t(diff) %*% solve(S) %*% (diff) )
#     }else{ # or else NA
#       D[i]=NA
#     }
#   }
#   return(D)
} # end KernelDensityML

