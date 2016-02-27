#'  KE Lotterhos
#'  March 18, 2015
#'  Compare Manhattan plots
#'  @param dfv2 is a dataframe output from the Getdf() function
#'  @param colorVect is a vector of colors for the points
#'  @param ind is a vector of indexes to be included in the plot (i.e. 9500:9700)
#'  @author KE Lotterhos

ComparePlot <- function(dfv2, colorVect=NULL, ind=NULL){

  par(mfrow=c(6,1), mar=c(3,4,1,1), bty="l")

  if(length(colorVect)==0){colorVect = rep(1, nrow(dfv2))}

  if(length(ind)==0){ind = 1:nrow(dfv2)}

  ### Hclust ######
  plot(ind, dfv2$Hcd[ind], col=colorVect[ind], pch=19, ylab = "hclust")

  ### PCS ######
  try(plot(ind, dfv2$pcs[ind], col=colorVect[ind], pch=19, ylab = "Fast PCS"))

  ### Mahalanobis distance ######
  # Md <- Mahalanobis(dfv2, colnums)
  #str(Md)
  try(plot(ind, dfv2$Md[ind], col=colorVect[ind], pch=19, ylab="Mahalanobis"))

  ### Harmonic mean ######
  try(plot(ind, dfv2$Hd[ind], col=colorVect[ind], pch=19, ylab = "Harmonic mean distance"))

  ### KernelDens ML ######
  try(plot(ind, dfv2$Kd[ind], col=colorVect[ind], pch=19, ylab = "Kernel density (ML)"))

  ### Nearest neighbor ######
  try(plot(ind, dfv2$Nd[ind], col=colorVect[ind], pch=19, ylab = "Nearest neighbor"))

 }
