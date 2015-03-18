
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

  ### Mahalanobis distance ######
  Md <- Mahalanobis(dfv2, colnums)
  str(Md)

  plot(Md[[3]][loci.ind], col=factor(dfv$s_high[loci.ind]))

  ### KernelDensSD ######
  Kd <- KernelDensSD(dfv2, colnums, 3)
  str(Kd)
  plot(Kd[[3]][loci.ind], col=factor(dfv$s_high[loci.ind]))

  ### Hclust ######
  Hcd <- hclust.ranking(dfv2, colnums)
  str(Hcd[[1]])
  str(Hcd[[2]])
  plot(Hcd$rank.outliers)
  plot(Hcd[[1]][loci.ind])
  plot(Hcd[[2]][loci.ind], col=factor(dfv$s_high[loci.ind]))

