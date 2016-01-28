
getwd()
source("R/DistanceFunctions.R")
source("R/ComparePlot.R")


#### One Refuge Simulation Example ######
  dfv <- read.table("inst/misc/OneRefSim.txt", header=TRUE)
  #dfv2 <- dfv[dfv$SNPIncluded,]
  colnums <- c(11, 12, 16)
  loci.ind <- c(9500:9996)
  #head(dfv)
  #str(dfv)
  #cbind(colnames(dfv))

  table(dfv2$s_high)  
  names(dfv)[colnums]
  dfv2.out <- Getdf(dfv, colnums)
  dim(dfv)
  dim(dfv2.out)
  head(dfv2.out)
  pdf(file = "~/Google Drive/MultiOutlierVisualization/practiceData/KatieSims1R.pdf",width = 4, height=6)
    par(mfrow=c(2,1), mar=c(3,4,1,1))
    plot(dfv2.out[,11], dfv2.out[,12], col=factor(dfv2$s_high), xlab="Spearman's rho (GEA)", ylab="XTX (Fst analog)")
    plot(dfv2.out[,11], dfv2.out[,16], col=factor(dfv2$s_high), xlab="Spearman's rho (GEA)", ylab="Z-score (LFMM, GEA)")
  dev.off()
  pdf(file = "~/Google Drive/MultiOutlierVisualization/practiceData/KatieSims1R_multiv.pdf",width = 6, height=8)
    ComparePlot(dfv2.out, colorVect=factor(dfv2$s_high), 9500:9996)
  dev.off()
##################################################

##################################################
#### Non-parametric example with toy data ######
  dfv3 <- read.table("inst/misc/NonParamEx.txt", header=TRUE)
  dfv3.out <- Getdf(dfv3, c(1,2))
  head(dfv3)
  pdf("~/Google Drive/MultiOutlierVisualization/practiceData/NonParamEx.pdf", width = 4, height = 4)
    plot(dfv3$x2, dfv3$y2, col=c(rep(1,10000),3), pch=19)
  dev.off()
  png("~/Google Drive/MultiOutlierVisualization/practiceData/NonParamEx_multiV.png", width = 6, height = 8, res=300, units="in")
    ComparePlot(dfv3.out, colorVect=c(rep(grey(0.8),10000),"blue"), ind=NULL)
  dev.off()
##################################################

##################################################
#### Liuyang's data ######
  dfv4 <- read.table("~/Google Drive/MultiOutlierVisualization/practiceData/toyExample_Liuyang.txt", sep= "\t", header=TRUE)
  dfv4.out <- Getdf(dfv4, c(4,6,8))
  head(dfv4.out)
  ind<- 17250:17400
  
  pdf("~/Google Drive/MultiOutlierVisualization/practiceData/Liuyang_multiV.pdf", width = 4, height = 8)
    ComparePlot(dfv4.out, ind=ind)
  dev.off()

  pdf("~/Google Drive/MultiOutlierVisualization/practiceData/Liuyang_betas.pdf", width = 4, height = 8)
    par(mfrow=c(3,1), mar=c(4,4,1,1))
    plot(ind, (dfv4.out$Trait1_Beta)[ind])
    plot(ind, (dfv4.out$Trait2_Beta)[ind])
    plot(ind, (dfv4.out$Trait3_Beta)[ind])
  dev.off()

  pdf("~/Google Drive/MultiOutlierVisualization/practiceData/Liuyang_p.pdf", width = 4, height = 8)
    par(mfrow=c(3,1), mar=c(4,4,1,1))
    plot(ind, -log(dfv4.out$Trait1_P)[ind])
    plot(ind, -log(dfv4.out$Trait2_P)[ind])
    plot(ind, -log(dfv4.out$Trait3_P)[ind])
  dev.off()

##################################################

##################################################
#### Sean's data ######
  dfv5 <- read.csv("~/Google Drive/MultiOutlierVisualization/practiceData/butternut_pops_summ_stats_Hoban.csv")#, sep= "\t", header=TRUE)
  plot(dfv5$A1, dfv5$Ar2)#, data=dfv5)
  plot(dfv5$HE3, dfv5$HO)#, data=dfv5)
  plot(dfv5$FST4, dfv5$RST)#, data=dfv5)
  head(dfv5.out)
  dfv5.out <- Getdf(dfv4, c(3:7))
  head(dfv5.out)
  ComparePlot(dfv5.out)
##################################################


##################################################
