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
  
  par(mfrow=c(4,1), mar=c(3,4,0,1), bty="l")
  
  if(length(colorVect)==0){colorVect = rep(1, nrow(dfv2))}
  
  if(length(ind)==0){ind = 1:nrow(dfv2)}
  
  ### Mahalanobis distance ######
 # Md <- Mahalanobis(dfv2, colnums)
  #str(Md)
  try(plot(ind, dfv2$Md.mlp[ind], col=colorVect[ind], pch=19, ylab="Mahalanobis"))
 
  ### PCS ######
  if(any(is.numeric(dfv2$pcs.mlp))){
  try(plot(ind, dfv2$pcs.mlp[ind], col=colorVect[ind], pch=19, ylab = "Fast PCS"))
  }
  ### KernelDensSD ######
  #Kd <- KernelDensSD(dfv2, colnums, 1.5)
  try(plot(ind, dfv2$Kd.mlp[ind], col=colorVect[ind], pch=19, ylab="Kernel SD"))
  
  ### Hclust ######
  #Hcd <- hclust.ranking(dfv2, colnums)
  #plot(ind, dfv2$Hcd.mlp[ind], col=colorVect[ind], pch=19, ylab = "hclust")

  ### KernelDens ML ######
  try(plot(ind, dfv2$Kd.ML.mll[ind], col=colorVect[ind], pch=19, ylab = "Kernel ML"))
  

 }

#'  KE Lotterhos
#'  March 18, 2015
#'  Get a dataframe with all multivariate stats
#'  @param dfv2 is a dataframe containing observations in rows and statistics in columns
#'  @param colnums is a vector of column numbers on which to compute the multivar stat
#'  @param alpha is for the FastPCS function
#'  @param n.sd is the number of standard deviations for the kernel size in the KernelDensSD
#'  @author KE Lotterhos

Getdf <- function(dfv2, colnums, n.sd=1.5, alpha=0.5, whichfun = "all"){
  ### Remove NAs
  if (any(is.na(dfv2[,colnums]))) {
    rows.keep <- apply(!is.na(dfv2[,colnums]), 1, any)
    dfv2 <- dfv2[rows.keep,]
    writeLines("Rows with NAs were removed")
  }
  
  ### Check for duplicated rows and abort
  if (any(duplicated(dfv2))) {
    writeLines("Error: Your data frame has duplicated rows")
    dfv2[duplicated(dfv2),]
    break()
  }
  
  writeLines("Calculating outlierliness based on FastPCS...")    
    x<- system.time({
      tx <- try(pcs<-FastPCS.out(dfv2, colnums, alpha))
      if("try-error" %in% class(tx)){
        pcs <- list(D.pcs = NA, D.pcs.rank = NA, minus.log.emp.p = NA)
      }
    })
    print(x)
  
  writeLines("Calculating outlierliness based on Mahalanobis distance...")
    x<- system.time({
      tx <- try(Md <- Mahalanobis(dfv2, colnums))
      if("try-error" %in% class(tx)){Md <- list(Dm = NA, Dm.rank = NA, minus.log.emp.p = NA)}
      })
    print(x)
  
  writeLines("Calculating outlierliness based on kernel density and standard deviation...")
    x<- system.time({
      tx <- try(Kd <- KernelDensSD(dfv2, colnums, n.sd))
      if("try-error" %in% class(tx)){Kd <- list(empDens = NA, Dk.rank = NA, minus.log.emp.p = NA)}
      })
    print(x)
  
#   writeLines("Calculating outlierliness based on clustering (outlier.ranking in DmWR)...")
#     x<- system.time({
#       tx <- try(Hcd <- hclust.ranking(dfv2, colnums))
#       if("try-error" %in% class(tx)){
#         Hcd <- list(empDens = NA, Dk.rank = NA, minus.log.emp.p = NA)
#       }
#     })
#     print(x)

  writeLines("Calculating outlierliness based on kernel density and maximum likelihood...")  
    x<- system.time({
      tx <- try(Kd.ML <- KernelDensityML(dfv2, colnums))
      if("try-error" %in% class(tx)){Kd.ML <- NA}
    })
    print(x)


  return(data.frame(dfv2, Md=Md$Dm, Md.rank=Md$Dm.rank, Md.mlp=Md$minus.log.emp.p,
                    Kd=Kd$empDens, Kd.rank=Kd$Dk.rank, Kd.mlp=Kd$minus.log.emp.p,
                    #Hcd.rank=Hcd$h.rank, Hcd.mlp=Hcd$minus.log.p,
                    Kd.ML.mll=Kd.ML$Kd.ML.mll, Kd.ML.rank= Kd.ML$Kd.ML.rank, Kd.ML.mlp=Kd.ML$minus.log.emp.p,
                    pcs.d = pcs$D.pcs, pcs.rank=pcs$D.pcs.rank, pcs.mlp=pcs$minus.log.emp.p)
         )
}