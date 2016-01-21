
### KE Lotterhos
### March 30, 2015
source("R/DistanceFunctions.R")
source("R/ComparePlot.R")

#### Allan Strand's data ######
 dfv6 <- read.csv("~/Google Drive/MultiOutlierVisualization/practiceData/allanstrand-cline-parameters.csv")#, sep= "\t", header=TRUE)
 head(dfv6)
 plot(dfv6$slope, dfv6$mid)
  hist(dfv6$slope)

 plot(dfv6$mid, dfv6$slope) 
  dfv6$log.slope <- log(abs(dfv6$slope))
plot(dfv6$mid, dfv6$log.slope)
  head(dfv6)
  cs <- which(names(dfv6.out) %in% c("log.slope", "mid"))
  dfv6.out <- Getdf(dfv6, cs)
    head(dfv6.out)
  ComparePlot(dfv6.out)

  ### Top hits 
  top.md <- dfv6.out[dfv6.out$Md.rank <= 10,]
  top.pcs <- dfv6.out[dfv6.out$pcs.rank <= 10,]
  top.kd <- dfv6.out[dfv6.out$Kd.ML.rank <= 10,]
  
  top <- rbind(top.md, top.kd)
  top <- top[-which(duplicated(top$locus)),]
  write.table(top,"~/Google Drive/MultiOutlierVisualization/practiceData/AllanTopHits.csv" ,sep = ", ")

pdf("~/Google Drive/MultiOutlierVisualization/practiceData/AllanStrandOutliers.pdf",
    width=4, height=6)
  par(mfrow=c(1,1), mar = c(4,4,1,1))
  plot(dfv6$mid, dfv6$log.slope, xlab="Midpoint of cline", ylab="Ln(Abs(Slope))",
       ylim=c(-9,5))
  points(top.md$mid, top.md$log.slope, pch=19, col="black")
  points(top.pcs$mid, top.pcs$log.slope, pch=6, col="blue", cex=1.5)
  points(top.kd$mid, top.kd$log.slope, pch=2, col = "red", cex=1.5)
  legend(33, 5, bty="n", legend=c("Mahalanobis", "PCSfast", "Kernel ML"),
         pch=c(19, 6,2), col=c("black", "blue", "red"))
dev.off()
