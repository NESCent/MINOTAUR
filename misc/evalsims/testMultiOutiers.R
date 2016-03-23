
getwd()
source("misc/evalsims/distanceFunctionsOther.R")
#source("misc/evalsims/ComparePlot.R")
source("misc/evalsims/GetAllMultiStats.R")
install.packages("devtools", dependencies=TRUE)
library(devtools)
install_github("NESCent/MINOTAUR")
library(MINOTAUR)

#### Non Parameteric Simulation Example ######
  np <- read.csv("misc/evalsims/nonPara_simData.csv")
  head(np)
  np2 <- Getdf(np)
  col <- rep("grey", nrow(np2))
  pch <- rep(19, nrow(np2))
  col[nrow(np2)] <- "blue"
  pch[nrow(np2)] <- 17
  ind <- c(1:999/1000,1.01)
 png("misc/evalsims/nonPara_log.png", width=3, height=8, res=450, units="in")
  par(mfrow=c(4,1), mar=c(3,4,1,1), bty="l")
    plot(ind,log(np2$Md), col=col, pch=pch, ylab= "Mahalanobis")
      #abline(sort(log(dfv3.out$Md[dfv3.out$s==0]))[9900*0.999],0)
      text(0,1.5, "A", cex=2)

    plot(ind,log(np2$Hd), col=col, pch=pch, ylab= "Harmonic mean dist.")
      #abline(sort(log(dfv3.out$Hd[dfv3.out$s==0]))[9900*0.999],0)
      text(0,1.5, "B", cex=2)
      #text(0,2.3,round(getEmpPower(dfv3.out$Hd,dfv3.out$s_high==0),2))

    plot(ind, log(np2$Kd), col=col, pch=pch, ylab= "Kernel density")
      #abline(sort(log(dfv3.out$Kd[dfv3.out$s==0]))[9900*0.999],0)
      text(0,4.0, "C", cex=2)
      #text(0,4.1,round(getEmpPower(dfv3.out$Kd,dfv3.out$s_high==0),2))

    plot(ind, log(np2$Nd), col=col, pch=pch, ylab= "Nearest neighbor")
      #abline(sort(log(dfv3.out$Nd[dfv3.out$s==0]))[9900*0.999],0)
      text(0,0, "D", cex=2)
      #text(0,0.1,round(getEmpPower(dfv3.out$Nd,dfv3.out$s_high==0),2))
  dev.off()

#### One Refuge Simulation Example ######
  d1 <- read.table("~/Google Drive/MultiOutlierVisualization/practiceData/KatieSims/2R_R30_1351142970_988_6_NumPops=30_NumInd=20Bayenv2LFMMpca.Cpval", header=TRUE)
  head(d1)
  dfv <- d1[c(1,3,4,5,10,12,13,15:17,34)]
  dim(dfv)
  head(dfv)
  #dfv2 <- dfv[dfv$SNPIncluded,]
  colnums <- 9:11
  #head(dfv)
  #str(dfv)
  #cbind(colnames(dfv))

  table(dfv$s_high)
  names(dfv)[colnums]
  dfv2.out <- Getdf(dfv, colnums)
  dim(dfv)
  dim(dfv2.out)
  head(dfv2.out)
  dfv3.out <- dfv2.out[,-c(12:13)]
 write.table(dfv3.out, "data/TwoRefSimForShiny.txt",row.names=FALSE)


  head(dfv3.out)
   quartz()
  col <- factor(dfv2.out$s_high)
  levels(col) = c("grey",  "#9ad0f3", "#0072B2", "#D55E00")
  col <- as.character(col)
  ind <- c(1:9900/100, 100:199)
  png("misc/evalsims/TwoRef_log.png", width=6, height=10, res=450, units="in")
   par(mfrow=c(4,1), mar=c(3,4,1,1), bty="l")
    plot(ind, log(dfv3.out$Md), col=col, pch=19, ylab= "Mahalanobis")
      abline(sort(log(dfv3.out$Md[dfv3.out$s==0]))[9900*0.999],0)
      text(0,2.5, "A", cex=2)
      text(0,2.1,round(getEmpPower(dfv3.out$Md,dfv3.out$s_high==0),2))

    plot(ind, log(dfv3.out$Hd), col=col, pch=19, ylab= "Harmonic mean dist.")
      abline(sort(log(dfv3.out$Hd[dfv3.out$s==0]))[9900*0.999],0)
      text(0,2.5, "B", cex=2)
      text(0,2.3,round(getEmpPower(dfv3.out$Hd,dfv3.out$s_high==0),2))

    plot(ind, log(dfv3.out$Kd), col=col, pch=19, ylab= "Kernel density")
      abline(sort(log(dfv3.out$Kd[dfv3.out$s==0]))[9900*0.999],0)
      text(0,4.5, "C", cex=2)
      text(0,4.1,round(getEmpPower(dfv3.out$Kd,dfv3.out$s_high==0),2))

    plot(ind, log(dfv3.out$Nd), col=col, pch=19, ylab= "Nearest neighbor")
      abline(sort(log(dfv3.out$Nd[dfv3.out$s==0]))[9900*0.999],0)
      text(0,0.5, "D", cex=2)
      text(0,0.1,round(getEmpPower(dfv3.out$Nd,dfv3.out$s_high==0),2))

  dev.off()

  png("misc/evalsims/TwoRefUnivar.png", width=6, height=10, res=450, units="in")
    par(mfrow=c(4,1), mar=c(3,4,1,1), bty="l")
    plot(ind, dfv3.out$log.bf, col=col, pch=19, ylab= "Bayenv log(BF)")
      abline(sort(dfv3.out$log.bf[dfv3.out$s==0])[9900*0.999],0)
      text(0,14, "A", cex=2)
      text(0,13,round(getEmpPower(dfv3.out$log.bf,dfv3.out$s_high==0),2))

    plot(ind, dfv3.out$rho, col=col, pch=19, ylab= "Bayenv rho")
      abline(sort(dfv3.out$rho[dfv3.out$s==0])[9900*0.999],0)
     text(0,0.45, "B", cex=2)
      text(0,0.4,round(getEmpPower(dfv3.out$rho,dfv3.out$s_high==0),2))

    plot(ind, log(dfv3.out$xtx), col=col, pch=19, ylab= "Bayenv log(XTX)")
      abline(sort(log(dfv3.out$xtx[dfv3.out$s==0]))[9900*0.999],0)
     text(0, 4.25, "C", cex=2)
      text(0,4.19,round(getEmpPower(dfv3.out$xtx,dfv3.out$s_high==0),2))

    plot(ind, log(dfv3.out$TW.Zscore), col=col, pch=19, ylab= "LFMM Z-score", ylim=c(-9,3))
      abline(sort(log(dfv3.out$TW.Zscore[dfv3.out$s==0]))[9900*0.999],0)
      text(0, 3, "D", cex=2)
      text(0,2.3,round(getEmpPower(dfv3.out$TW.Zscore,dfv3.out$s_high==0),2))
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
  colnames(dfv4.out)[11:13] <- paste("Trait", 1:3, "minusLogP", sep="")
  dfv4.out <- dfv4.out[,-c(14:15)]
  write.table(dfv4.out, "data/HumanGWASForShiny.txt", row.names=FALSE)
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
#### Alan Strand Example ######
  dfv5 <- read.csv("data/allanstrand-cline-parameters.csv")
  head(dfv5)
  tail(dfv5)
  dfv5.out <- Getdf(dfv5,2:4)
  head(dfv5.out)
  col <- rep("grey",nrow(dfv5.out))
  dfv5.out <- dfv5.out[order(dfv5.out$p.val,decreasing = TRUE),]
   col[dfv5.out$p.val<1e-05]="blue"
  png("misc/evalsims/Strand.png", width = 6, height = 8, res=450, units="in")
  ComparePlot(dfv5.out, col)
  dev.off()
