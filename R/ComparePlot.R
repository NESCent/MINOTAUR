source("R/DistanceFunctions.R")
source("R/KernelDensityML.R")

#'  KE Lotterhos
#'  March 18, 2015
#'  Compare Manhattan plots
#'  @param dfv2 is a dataframe output from the Getdf() function
#'  @param colorVect is a vector of colors for the points
#'  @param ind is a vector of indexes to be included in the plot (i.e. 9500:9700)
#'  @author KE Lotterhos

ComparePlot <- function(dfv2, colorVect=NULL, ind=NULL){
  
  par(mfrow=c(5,1), mar=c(3,4,0,1), bty="l")
  
  if(length(colorVect)==0){colorVect = rep(1, nrow(dfv2))}
  
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

 ### KernelDens ML ######
 plot(ind, dfv2$pcs.mlp[ind], col=colorVect[ind], pch=19, ylab = "Fast PCS")
 
 }

#'  KE Lotterhos
#'  March 18, 2015
#'  Get a dataframe with all multivariate stats
#'  @param dfv2 is a dataframe containing observations in rows and statistics in columns
#'  @param colnums is a vector of column numbers on which to compute the multivar stat
#'  @param alpha is for the FastPCS function
#'  @param n.sd is the number of standard deviations for the kernel size in the KernelDensSD
#'  @author KE Lotterhos

Getdf <- function(dfv2, colnums, n.sd=1.5, alpha=0.5){
  writeLines("Calculating outlierliness based on Mahalanobis distance...")
  Md <- Mahalanobis(dfv2, colnums)
  writeLines("Calculating outlierliness based on kernel density and standard deviation...")
  Kd <- KernelDensSD(dfv2, colnums, n.sd)
  writeLines("Calculating outlierliness based on clustering (outlier.ranking in DmWR)...")
  Hcd <- hclust.ranking(dfv2, colnums)
  writeLines("Calculating outlierliness based on kernel density and maximum likelihood...")  
  Kd.ML <- KernelDensityML(dfv2,colnums)
  writeLines("Calculating outlierliness based on FastPCS...")    
  pcs <- FastPCS.out(dfv2, colnums, alpha)
  return(data.frame(dfv2, Md=Md$Dm, Md.rank=Md$Dm.rank, Md.mlp=Md$minus.log.emp.p,
                    Kd=Kd$empDens, Kd.rank=Kd$Dk.rank, Kd.mlp=Kd$minus.log.emp.p,
                    Hcd.rank=Hcd$h.rank, Hcd.mlp=Hcd$minus.log.p,
                    Kd.ML.mll=Kd.ML,
                    pcs.d = pcs$D.pcs, pcs.rank=pcs$D.pcs.rank, pcs.mlp=pcs$minus.log.emp.p))
}