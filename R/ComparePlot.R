source("R/DistanceFunctions.R")

ComparePlot <- function(dfv2, colorVect=1, ind=NULL){
  
  par(mfrow=c(3,1), mar=c(3,4,1,1), bty="l")
  
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
}

Getdf <- function(dfv2, colnums, n.sd=1.5){
  Md <- Mahalanobis(dfv2, colnums)
  Kd <- KernelDensSD(dfv2, colnums, n.sd)
  Hcd <- hclust.ranking(dfv2, colnums)
  return(data.frame(dfv2, Md=Md$Dm, Md.rank=Md$Dm.rank, Md.mlp=Md$minus.log.emp.p,
                    Kd=Kd$empDens, Kd.rank=Kd$Dk.rank, Kd.mlp=Kd$minus.log.emp.p,
                    Hcd.rank=Hcd$h.rank, Hcd.mlp=Hcd$minus.log.p))
}