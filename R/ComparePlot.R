source("R/DistanceFunctions.R")
source("R/KernelDensityML.R")

#'  KE Lotterhos
#'  March 18, 2015
#'  Compare Manhattan plots
#'  @param dfv2 is a dataframe containing observations in rows and statistics in columns
#'  @param colorVect is a vector of colors for the points
#'  @param ind is a vector of indexes to be included in the plot (i.e. 9500:9700)
#'  @author KE Lotterhos

ComparePlot <- function(dfv2, colorVect=NULL, ind=NULL){
  
  par(mfrow=c(4,1), mar=c(3,4,1,1), bty="l")
  
  if(length(ColorVect)==0){colorVect = rep(1, nrow(dfv2))}
  
  if(length(ind)==0){ind = 1:nrow(dfv2)}
  
  ### Mahalanobis distance ######
 # Md <- Mahalanobis(dfv2, colnums)
  #str(Md)
  plot(ind, dfv2$Md.mlp[ind], col=colorVect[ind], pch=19, ylab="Mahalanobis")

  ### KernelDensSD ######
  #Kd <- KernelDensSD(dfv2, colnums, 1.5)
  plot(ind, dfv2$Kd.mlp[ind], col=colorVect[ind], pch=19, ylab="Kernel SD")
  
  ### Hclust ######
  #Hcd <- hclust.ranking(dfv2, colnums)
  plot(ind, dfv2$Hcd.mlp[ind], col=colorVect[ind], pch=19, ylab = "hclust")

  ### KernelDens ML ######
  plot(ind, dfv2$Kd.ML.mll[ind], col=colorVect[ind], pch=19, ylab = "Kernel ML")
}

#'  R Verity
#'  March 18, 2015
#'  calculate density of point i from all other points
#'  @param df is a dataframe containing observations in rows and statistics in columns
#'  @param i is the row number that will be compared against all other rows
#'  @param sigma is the standard deviation of the normal kernel in all dimensions
#'  @author R Verity

Getdf <- function(dfv2, colnums, n.sd=1.5){
  Md <- Mahalanobis(dfv2, colnums)
  Kd <- KernelDensSD(dfv2, colnums, n.sd)
  Hcd <- hclust.ranking(dfv2, colnums)
  Kd.ML <- KernelDensityML(dfv2,colnums)
  return(data.frame(dfv2, Md=Md$Dm, Md.rank=Md$Dm.rank, Md.mlp=Md$minus.log.emp.p,
                    Kd=Kd$empDens, Kd.rank=Kd$Dk.rank, Kd.mlp=Kd$minus.log.emp.p,
                    Hcd.rank=Hcd$h.rank, Hcd.mlp=Hcd$minus.log.p,
                    Kd.ML.mll=Kd.ML))
}