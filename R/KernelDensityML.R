
#'  R Verity
#'  March 18, 2015
#'  calculate density of point i from all other points
#'  @param df is a dataframe containing observations in rows and statistics in columns
#'  @param i is the row number that will be compared against all other rows
#'  @param sigma is the standard deviation of the normal kernel in all dimensions
#'  @author R Verity
#'  @keywords internal

## .logLike_internal ##
# calculate density of point i from all other points
.logLike_internal = function(df,i,sigma) {
  rawProbs = rep(0,nrow(df)-1)
  for (j in 1:ncol(df)) {
    rawProbs = rawProbs + dnorm(df[i,j],df[-i,j],sd=sigma[j],log=TRUE)
  }
  totalProb = log(mean(exp(rawProbs)))
  return(totalProb)
} # end .logLike_internal


#'  R Verity
#'  March 18, 2015
#'  calculate kernel density of target point given anchor points
#'  @param target is a vector of points on which the density will be calculated
#'  @param anchor is a data frame of points used to produce kernel density
#'  @param sigma is the standard deviation of the normal kernel in all dimensions
#'  @author R Verity
#'  @keywords internal

## .logLike_external ##
# calculate kernel density of target point given anchor points
.logLike_external = function(target,anchor,sigma) {
  rawProbs = rep(0,nrow(anchor))
  for (j in 1:length(target)) {
    rawProbs = rawProbs + dnorm(target[j],anchor[,j],sd=sigma[j],log=TRUE)
  }
  totalProb = log(mean(exp(rawProbs)))
  return(totalProb)
} # end .logLike_external


#'  R Verity
#'  March 18, 2015
#'  calculate deviance (-2*logLikelihood) of all points in data frame based on the internal kernel density
#'  @param df is a dataframe containing observations in rows and statistics in columns
#'  @param lambda is a scaling factor on the bandwidth (final bandwidth = lambda*sigma)
#'  @param sigma is the standard deviation of the normal kernel in all dimensions
#'  @author R Verity
#'  @keywords internal

## .deviance_internal ##
# calculate deviance (-2*logLikelihood) of all points in data frame based on the internal kernel density
.deviance_internal = function(df,lambda,sigma) {
  dev <- -2*sum(mapply(.logLike_internal,i=1:nrow(df),MoreArgs=list(df=df,sigma=lambda*sigma)))
  return(dev)
}


#'  R Verity
#'  March 18, 2015
#'  calculate kernel density distance with maximum likelihood bandwidth selection
#'  @rdname KernelDensityML
#'  @name KernelDensityML
#'  @param dfv is a dataframe containing the observations in rows and the statistics in columns
#'  @param column.nums is the column numbers of the dataframe containing the statistics used to calculate kernel density distance
#'  @author R Verity
#'  @export KernelDensityML
#'  @examples
#'  ##TO DO

## KernelDensityML ##
# calculate kernel density distance with maximum likelihood bandwidth selection
KernelDensityML <- function(dfv, column.nums){
  # This function produces a kernel density from the selected points, and returns the kernel 'distance'
  # as -2*logLikelihood of all points under this model. Kernel bandwidth is chosen automatically by
  # maximum likelihood.
  # Nb. In theory can work with missing data, but not coded up yet
  
  # subset and subsample data
  df.vars <- dfv[,column.nums,drop=FALSE]
  nvars <- length(column.nums)
  nlocs <- nrow(df.vars)
  df.subSample <- df.vars[sample(nlocs,min(200,nlocs)),,drop=FALSE]
  
  # calculate standard deviation of each variable
  sigma = apply(df.vars,2,function(x){sd(x)})
  
  # estimate optimal bandwidth by maximum likelihood
  optim_results <- optim(par=1,.deviance_internal,df=df.subSample,sigma=sigma,method='Brent',lower=0,upper=10)
  ml_bandwidth <- optim_results$par*sigma
  
  # calculate deviance under optimal bandwidth
  kernelDist <- -2*apply(df.vars,1,function(x){.logLike_external(x,df.subSample,ml_bandwidth)})
  
  Dm.rank <- nlocs-rank(kernelDist, na.last="keep")+1
  minus.log.emp.p <- -log(Dm.rank/(nlocs-sum(is.na(kernelDist))))
  
  return(list(Kd.ML.mll = kernelDist, Kd.ML.rank=Dm.rank, minus.log.emp.p=minus.log.emp.p))
  
} # end KernelDensityML


# n = 1000
# d = 10
# X1 = rmvnorm(n/2,mean=rnorm(d),sigma=diag(d)+(1-diag(d))*0.1)
# X2 = rmvnorm(n/2,mean=rnorm(d),sigma=diag(d)+(1-diag(d))*-0.1)
# X = rbind(X1,X2)
# X[,1] = X[,1]/10
# plot(X[,1],X[,2])
# kernelDist <- KernelDensityML(dfv=X,column.nums=1:ncol(X))
# rankOrder = order(kernelDist,decreasing=TRUE)
# colVec = rep('grey',length(kernelDist))
# colVec[rankOrder][1:10] = 'red'
# plot(X[,1],X[,2],col=colVec)
