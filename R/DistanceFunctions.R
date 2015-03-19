
############# Mahalanobis distance #################
#'  KE Lotterhos
#'  March 19, 2015
#'  calculate Malanahobis distance
#'  @param dfv is a dataframe containing the observations in rows and the statistics in columns
#'  @param column.nums is the column numbers of the dataframe containing the statistics used to calculate Mahalanobis distance
#'  @author KE Lotterhos
#'  @export Mahalanobis

Mahalanobis <- function(dfv, column.nums){
# This function calculates the Mahalanobis distance for each row (locus, SNP) in the dataframe
# dfv is a dataframe with each row a locus or population, and columns statistics and other information
# column.nums is the columns in the dataframe to be used for analysis
# haven't tested with missing data
  
  if(sum(is.na(dfv))>0){print("Error: please input a dataframe with no 
                              NAs in the variables used to calculate the 
                              multivariate summary statistic"); break()}
  
  df.vars <- as.matrix(dfv[,column.nums])
  class(df.vars) <- "numeric"
  nlocs <- nrow(df.vars)
  
  mu <- t(t(colMeans(df.vars, na.rm=TRUE))) 
    # calculate means of each variable and make it a column vector
  S <- cov(df.vars, use="pairwise.complete.obs") 
    # calculate variance-covariance matrix
  
  D <- rep(NA, nlocs) # Mahalanobis distance
  for (i in 1:nrow(df.vars)){
    x <- t(t(df.vars[i,]))
    diff <- x-mu
    D[i] <- sqrt( t(diff) %*% solve(S) %*% (diff) )
  }
  Dm.rank <- nlocs-rank(D, na.last="keep")+1
  minus.log.emp.p <- -log(Dm.rank/(nlocs-sum(is.na(D))))
  
  return(list(Dm=D, Dm.rank=Dm.rank, minus.log.emp.p=minus.log.emp.p))
} # end Mahalanobis


############# Hclust ranking #################
#'  KE Lotterhos
#'  March 19, 2015
#'  calculate multivariate distance based on outliers.ranking function in package DMwR
#'  @param dfv is a dataframe containing the observations in rows and the statistics in columns
#'  @param column.nums is the column numbers of the dataframe containing the statistics used to calculate Mahalanobis distance
#'  @author KE Lotterhos
#'  @export hclust.ranking

### The outliers.ranking function in package DMwR
if (("DMwR" %in% installed.packages())==FALSE) install.packages("DMwR")
library(DMwR)

hclust.ranking <- function(dfv, column.nums){
  # This function calculates the outlier distance for each row (locus, SNP) in the dataframe based on the hclust function
  # dfv is a dataframe with each row a locus or population, and columns statistics and other information
  # column.nums is the columns in the dataframe to be used for analysis
  # haven't tested with missing data
  if(sum(is.na(dfv))>0){print("Error: please input a dataframe with no 
                              NAs in the variables used to calculate the 
                              multivariate summary statistic"); break()}
  
  data1 <- as.matrix(dfv[,column.nums])
  class(data1) <- "numeric"

  #nlocs <- nrow(dfv)
  hout <- outliers.ranking(data1, method = "sizeDiff",
                 method.pars = NULL,
                 clus = list(dist = "euclidean",alg = "hclust",
                             meth = "ward.D"),
                 power = 1, verb = F)
  return(list(h.rank = hout$rank.outliers, 
              minus.log.p = -log(1-hout$prob.outliers)))
} #end hclust.ranking

############# Kernel Density based on SD #################
#'  KE Lotterhos
#'  March 19, 2015
#'  calculate multivariate distance based on outliers.ranking function in package DMwR
#'  @param dfv is a dataframe containing the observations in rows and the statistics in columns
#'  @param column.nums is the column numbers of the dataframe containing the statistics used to calculate Mahalanobis distance
#'  @param n.sd is the number of standard deviations for the kernel size
#'  @author KE Lotterhos
#'  @export KernelDensSD

KernelDensSD <- function(dfv, column.nums, n.sd=1.5){
  ### This function takes a dataframe of statistics where each row is a locus/observation 
  ### and each column is an observation statistic
  ### It breaks up multivariate space into n-dimensional chunks, 
  ### with the size of each chunk determined by the standard deviation. 
  ### (n.sd) is the proportion of the standard deviation
  ### Next the density of points inside each chunk is calculated, and 
  ### chunks are ranked according to their density.  Ranks are used to create an empirical p-value
  if(sum(is.na(dfv))>0){print("Error: please input a dataframe with no 
                              NAs in the variables used to calculate the 
                              multivariate summary statistic"); break()}
  
  df.vars <- as.matrix(dfv[,column.nums])
  class(df.vars) <- "numeric"
  colnames(df.vars)<-NULL
  rownames(df.vars)<-NULL
  
  n.stat <- ncol(df.vars)
  nlocs <- nrow(df.vars)
  width <- apply(df.vars,2, sd, na.rm=TRUE)*n.sd
  
  empDens <- rep(NA, nrow(df.vars))
  tdf <- t(df.vars)
  
  for (i in 1:nlocs){
    vals <- df.vars[i,]
    vals_upper <- vals + width
    vals_lower <- vals - width
    temp <- colSums((tdf < vals_upper) & (tdf > vals_lower))==n.stat
    empDens[i] <- sum(temp, na.rm=TRUE)
  }
  Dk.rank <- rank(empDens,na.last="keep")
  minus.log.emp.p <- -log(Dk.rank/(nlocs-sum(is.na(empDens))))
  #plot(minus.log.emp.p)
  return(list(empDens=empDens, Dk.rank=Dk.rank, minus.log.emp.p=minus.log.emp.p))
} #end KernelDensSD



############### FastPCS ###############
#'  KE Lotterhos
#'  March 19, 2015
#'  calls FastPCS and formats output to be similar to other functions in this package
#'  @param dfv is a dataframe containing the observations in rows and the statistics in columns
#'  @param column.nums is the column numbers of the dataframe containing the statistics used to calculate Mahalanobis distance
#'  @param alpha is the parameter used is the FastPCS function
#'  @param seed is the parameter used is the FastPCS function
#'  @author KE Lotterhos
#'  @export FastPCS.out

if (("FastPCS" %in% installed.packages())==FALSE) install.packages("FastPCS")
library(FastPCS)

FastPCS.out <- function(dfv, column.nums, alpha=0.5, seed=NULL){
  if(sum(is.na(dfv))>0){print("Error: please input a dataframe with no 
                              NAs in the variables used to calculate the 
                              multivariate summary statistic"); break()}
    
  temp <- as.matrix(dfv[,column.nums])
  class(temp) <- "numeric"
  nlocs <- nrow(temp)
  out <- FastPCS(temp, nsamp=NULL, alpha=alpha, seed=NULL)

  D.pcs <- out$raw$distance
  D.pcs.rank <- nlocs - rank(D.pcs, na.last="keep") + 1
  minus.log.emp.p <- -log(D.pcs.rank/(nlocs-sum(is.na(D.pcs))))

  return(list(D.pcs=D.pcs, D.pcs.rank=D.pcs.rank, minus.log.emp.p=minus.log.emp.p))
}
