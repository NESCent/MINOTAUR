
getwd()
source("misc/evalsims/distanceFunctionsOther.R")
#source("misc/evalsims/ComparePlot.R")


#### One Refuge Simulation Example ######
  dfv <- read.table("data/OneRefSim.txt", header=TRUE)
  dim(dfv)
  #dfv2 <- dfv[dfv$SNPIncluded,]
  colnums <- c(10, 11, 12, 16)
  #head(dfv)
  #str(dfv)
  #cbind(colnames(dfv))

  table(dfv$s_high)
  names(dfv)[colnums]
  dfv2.out <- Getdf(dfv, colnums)
  dim(dfv)
  dim(dfv2.out)
  head(dfv2.out)
  quartz()
  col <- factor(dfv2.out$s_high)
  levels(col) = c("grey", "#0072B2",  "#9ad0f3", "#D55E00")
  col <- as.character(col)
  png("misc/evalsims/OneRef.png", width=6, height=10, res=450, units="in")
    ComparePlot(dfv2.out,colorVect = col)
  dev.off()
##################################################

##################################################
#### Non-parametric example with toy data ######
  dfv3 <- read.table("data/NonParamEx.txt", header=TRUE)
  dfv3.out <- Getdf(dfv3, c(1,2))
  head(dfv3)
  pdf("misc/evalsims/NonParamEx.pdf", width = 4, height = 4)
    plot(dfv3$x2, dfv3$y2, col=c(rep(1,10000),3), pch=19)
  dev.off()
  png("misc/evalsims/NonParamEx_multiV.png", width = 6, height = 10, res=450, units="in")
    ComparePlot(dfv3.out, colorVect=c(rep(grey(0.8),10000),"blue"), ind=NULL)
  dev.off()
##################################################

##################################################
#### Liuyang's data ######
  dfv4 <- read.csv("data/largeData.csv")
  minus.log.p <- -log(dfv4[,c(6,8,10)])
  dim(dfv4)
  dfv4 <- data.frame(dfv4, minus.log.p)
  dfv4.out <- Getdf(dfv4, 11:13) # p-value columns
  head(dfv4.out)
  ind<- 17250:17400

  png("misc/evalsims/Liuyang_smallData.png", width = 6, height = 10, res=450, units="in")
    ComparePlot(dfv4.out)
  dev.off()

  png("misc/evalsims/Liuyang_smallData_univar.png", width = 6, height = 8, res=450, units="in")
    par(mfrow=c(3,1), mar=c(4,4,1,1))
    plot((dfv4.out$Trait1_P.2))
    plot((dfv4.out$Trait2_P.2))
    plot((dfv4.out$Trait3_P.2))
  dev.off()

##################################################
