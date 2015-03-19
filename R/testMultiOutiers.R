
getwd()
source("R/DistanceFunctions.R")
source("R/ComparePlot.R")

  dfv <- read.table("inst/misc/OneRefSim.txt", header=TRUE)
  dfv2 <- dfv[dfv$SNPIncluded,]
  colnums <- c(11, 12, 16)
  loci.ind <- c(1:500, 9500:9996)
  head(dfv)
  str(dfv)
  cbind(colnames(dfv))
  dim(dfv)
  dim(dfv2)
  table(dfv2$s_high)  

  dfv2.out <- Getdf(dfv2, colnums)
  head(dfv2.out)
  plot(dfv2.out[,11], dfv2.out[,12], col=factor(dfv2$s_high))
  ComparePlot(dfv2.out, colorVect=factor(dfv2$s_high), 9500:9996)

  dfv3 <- read.table("inst/misc/NonParamEx.txt", header=TRUE)
  dfv3.out <- Getdf(dfv2, c(1,2))
  head(dfv3)
  plot(dfv3$x2, dfv3$y2)
  ComparePlot(dfv3.out, colorVect=c(rep(1,10000),3), ind=NULL)


  