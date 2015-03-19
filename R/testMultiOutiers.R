
getwd()
source("R/DistanceFunctions.R")
source("R/ComparePlot.R")

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
  dfv2.out <- Getdf(dfv2, colnums)
  head(dfv2.out)
  ComparePlot(dfv2.out, colorVect=factor(dfv2$s_high), ind=loci.ind)

  dfv2 <- read.table("inst/misc/NonParamEx.txt", header=TRUE)
  dfv2.out <- Getdf(dfv2, c(1,2))
  ComparePlot(dfv2.out, colorVect=c(rep(1,10000),3), ind=NULL)

