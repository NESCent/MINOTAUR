
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

hclust.ranking <- function(dfv, column.nums=1:ncol(dfv)){
  # This function calculates the outlier distance for each row (locus, SNP) in the dataframe based on the hclust function
  # dfv is a dataframe with each row a locus or population, and columns statistics and other information
  # column.nums is the columns in the dataframe to be used for analysis
  # haven't tested with missing data
  if(sum(is.na(dfv[,column.nums]))>0){print("Error: please input a dataframe with no NAs in the variables used to calculate the multivariate summary statistic"); break()}

  data1 <- as.matrix(dfv[,column.nums])
  class(data1) <- "numeric"

  #nlocs <- nrow(dfv)
  hout <- outliers.ranking(data1, method = "sizeDiff",
                 method.pars = NULL,
                 clus = list(dist = "euclidean",alg = "hclust",
                             meth = "ward.D"),
                 power = 1, verb = F)
  return(-log(1-hout$prob.outliers))
} #end hclust.ranking

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

FastPCS.out <- function(dfv, column.nums=1:ncol(dfv), alpha=0.5, seed=NULL){
  if(sum(is.na(dfv[,column.nums]))>0){print("Error: please input a dataframe with no NAs in the variables used to calculate the multivariate summary statistic"); break()}

  temp <- as.matrix(dfv[,column.nums])
  class(temp) <- "numeric"
  nlocs <- nrow(temp)
  out <- FastPCS(temp, nSamp=NULL, alpha=alpha, seed=NULL)

  D.pcs <- out$distance
  return(D.pcs)
}

############### ks package ###############
## TO DO
