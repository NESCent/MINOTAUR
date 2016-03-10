source("misc/evalsims/ComparePlot.R")
source("misc/evalsims/distanceFunctionsOther.R")
source("misc/evalsims/getEmpiricalP2.R")
library(qvalue)
install.packages("vioplot")
library(vioplot)

headdir <- "~/Google Drive/MultiOutlierVisualization/practiceData/KatieSims/"
resultsdir <- "~/Google Drive/MultiOutlierVisualization/practiceData/KatieSimsResults/"
filelist <- list.files(headdir)
filelist <- filelist[-55]
length(filelist)
if (!length(filelist)==72){print("Errr: missing simulation files")}


summarydf <- NULL
for (i in 1:length(filelist)){
  print(c(i, filelist[i]))
  infile <- paste(headdir, filelist[i], sep="")
  outfile <- paste(resultsdir, filelist[i], sep="")
  dat <- read.table(infile, header=TRUE)
  cols <- which(names(dat) %in% c("rho", "log.bf", "xtx", "TW.Zscore"))
  dat2 <- dat[dat$UseSNP,]
  dat.out <- Getdf(dat2, cols)
  #head(dat.out)
  #dim(dat.out)

  ### Make plots ####
#    pdf(file = paste(outfile, ".pdf", sep=""),width = 4, height=6)
#      par(mfrow=c(2,1), mar=c(4,4,1,1), oma=c(0,0,2,1))
#      plot(dat.out[,cols[1]], dat.out[,cols[2]], col=factor(dat.out$s_high), xlab="Spearman's rho (GEA)", ylab="XTX (Fst analog)")
#      plot(dat.out[,cols[1]], dat.out[,cols[3]], col=factor(dat.out$s_high), xlab="Spearman's rho (GEA)", ylab="Z-score (LFMM, GEA)")
#      mtext(dat.out$demog[1] , side = 3, outer=TRUE)
#    dev.off()
#    pdf(file = paste(outfile, "mulitD.pdf", sep=""),width = 6, height=8)
#      ComparePlot(dat.out, colorVect=factor(dat$s_high), 9500:9996)
#    dev.off()

  ### Calculate Empirical Power ###
    out <- data.frame(infile = as.character(infile),
             demog = as.character(dat.out$demog[1]),
             xtx.ep = getEmpPower(dat.out$xtx, dat.out$s_high==0),
             bf.ep = getEmpPower(dat.out$log.bf, dat.out$s_high==0),
             rho.ep = getEmpPower(dat.out$rho, dat.out$s_high==0),
             lfmm.ep = getEmpPower(dat.out$TW.Zscore, dat.out$s_high==0),
             Hcd.ep = getEmpPower(dat.out$Hcd, dat.out$s_high==0),
             pcs.ep = getEmpPower(dat.out$pcs, dat.out$s_high==0),
             Md.ep = getEmpPower(dat.out$Md, dat.out$s_high==0),
             Hd.ep = getEmpPower(dat.out$Hd, dat.out$s_high==0),
             Kd.ep = getEmpPower(dat.out$Kd, dat.out$s_high==0),
             Nd.ep = getEmpPower(dat.out$Nd, dat.out$s_high==0)
    )
  summarydf <- rbind(summarydf,out)
}

write.table(summarydf,file = paste(resultsdir,"LandsharcSummary.txt", sep=""))

colnames(summarydf)<-sub(".ep", "",colnames(summarydf))

pdf(paste(resultsdir,"LandsharcSummary.pdf", sep=""), width = 6, height = 8)
    par(mfrow=c(4,1), mar=c(3,3,1,1),oma=c(1,3,1,0))
    x <- 0.5
    y <- 1.1
    stats <- c(3:12)
    colors <- c(rep("magenta",4), rep("blue",1), rep("lightblue",3), rep("darkblue",3))
    boxplot(summarydf[summarydf$demog=="IM",stats], col=colors,
            ylim=c(0,1.2))
    text(x,y, "IM", cex=2)
    boxplot(summarydf[summarydf$demog=="IBD",stats], col=colors,
            ylim=c(0,1.2))
    text(x,y, "IBD", cex=2)
    boxplot(summarydf[summarydf$demog=="1R",stats], col=colors,
            ylim=c(0,1.2))
    text(x,y, "1R", cex=2)
    boxplot(summarydf[summarydf$demog=="2R",stats], col=colors,
            ylim=c(0,1.2))
    text(x,y, "2R", cex=2)
    mtext("Empirical Power", side=2, outer=TRUE)
dev.off()
# colMeans(summarydf[summarydf$demog=="IM",3:10])
# colMeans(summarydf[summarydf$demog=="IBD",3:10])
# colMeans(summarydf[summarydf$demog=="1R",3:10])
# colMeans(summarydf[summarydf$demog=="2R",3:10])
