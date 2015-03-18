
getwd()
source("R/DistanceFunctions.R")

  dfv <- read.table("inst/misc/OneRefSim.txt", header=TRUE)
  dfv2 <- dfv[dfv$SNPIncluded,]
  colnums <- c(11, 12, 16)
  loci.ind <- 9500:9996

  head(dfv)
  str(dfv)
  cbind(colnames(dfv))
  dim(dfv)
  dim(dfv2)
  table(dfv2$s_high)

  dfv2 <- read.table("inst/misc/NonParamEx.txt", header=TRUE)
  colnums <- c(1,2)

  ### Mahalanobis distance ######
  Md <- Mahalanobis(dfv2, colnums)
  str(Md)
  #plot(Md[[3]][loci.ind], col=factor(dfv$s_high[loci.ind]))
  plot(Md[[3]], col=c(rep(1,10000),3), pch=19)

  ### KernelDensSD ######
  Kd <- KernelDensSD(dfv2, colnums, 1.5)
  str(Kd)
  plot(Kd[[3]][loci.ind], col=factor(dfv$s_high[loci.ind]))
  plot(Kd[[3]], col=c(rep(1,10000),3), pch=19)
  

  ### Hclust ######
  Hcd <- hclust.ranking(dfv2, colnums)
  str(Hcd[[1]])
  str(Hcd[[2]])
  plot(Hcd$rank.outliers)
  plot(Hcd[[1]][loci.ind])
  plot(Hcd[[2]][loci.ind], col=factor(dfv$s_high[loci.ind]))

