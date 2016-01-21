
#'  R Verity
#'  May 15, 2015
#'  Calculate likelihood of each point from all others. Use this to calculate total deviance (-2 times total loglikelihood).
#'  @param df is a dataframe containing observations in rows and statistics in columns. Should already have been normalised to have same standard deviation in all dimensions.
#'  @param lambda is the standard deviation of the normal kernel in all dimensions
#'  @author R Verity
#'  @keywords deviance

## .leaveOneOutDeviance ##
# calculate total deviance
.leaveOneOutDeviance = function(df,lambda) {
  logProbMat = 0
  for (i in 1:ncol(df)) {
    distMat = dist(df[,i],diag=T,upper=T)
    logProbMat = logProbMat + dnorm(distMat,sd=lambda,log=T)
  }
  probMat = exp(logProbMat)
  pointDensity = rowSums.dist(probMat)
  output = -2*sum(log(pointDensity))
  return(output)
}


#'  R Verity
#'  May 15, 2015
#'  Calculate deviance of all points under kernel density distribution.
#'  @param df is a dataframe containing observations in rows and statistics in columns. Should already have been normalised to have same standard deviation in all dimensions.
#'  @param lambda is the standard deviation of the normal kernel in all dimensions
#'  @author R Verity
#'  @keywords deviance

## .pointDeviance ##
# calculate deviance of all points under kernel density distribution
.pointDeviance = function(df,lambda) {
  logProbMat = 0
  for (i in 1:ncol(df)) {
    distMat = dist(df[,i],diag=T,upper=T)
    logProbMat = logProbMat + dnorm(distMat,sd=lambda,log=T)
  }
  probMat = exp(logProbMat)
  pointDensity = rowSums.dist(probMat) + (1/sqrt(2*pi*lambda^2))^ncol(df)
  pointDensity = pointDensity/nrow(df)
  output = -2*log(pointDensity)
  return(output)
}


#'  R Verity
#'  May 15, 2015
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
  
  # extract basic properties of data
  df.vars <- dfv[,column.nums,drop=FALSE]
  nvars <- length(column.nums)
  nlocs <- nrow(df.vars)
  
  # calculate standard deviation in each dimension and divide through to normalise
  sigma = apply(df.vars,2,function(x){sd(x)})
  df.norm = df.vars
  for (i in 1:nvars) df.norm[,i] = df.norm[,i]/sigma[i]
  
  # obtain random sample with which to calculate optimal bandwidth
  set.seed(42)
  df.subSample <- df.norm[sample(nlocs,min(1000,nlocs)),,drop=FALSE]
  
  # estimate optimal bandwidth by maximum likelihood
  optim_results <- optim(par=1,.leaveOneOutDeviance,df=df.subSample,method='Brent',lower=0,upper=10)
  
  # calculate deviance under optimal bandwidth
  kernelDist <- .pointDeviance(lambda=optim_results$par,df=df.norm)
  
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
