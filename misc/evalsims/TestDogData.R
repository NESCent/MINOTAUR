
f <- read.csv("/Users/katie/Google Drive/MultiOutlierVisualization/dataForPub/all_chr_bulldog_french.csv")
head(f)
quartz()
  par(mfrow=c(5,1), mar=c(2,4,1,1))
  plot(f$hapflk)
  plot(f$unstd_ihs)
  plot(f$win51_H12)
  plot(f$win51_pi)
  plot(f$win51_TD)
col<-which(colnames(f) %in% c("hapflk", "unstd_ihs", "win51_H12"))
dfv <- f[,col]
f2 <- Getdf(dfv)
head(f2)
quartz()
ComparePlot(f2[,4:9])

col2<-which(colnames(f) %in% c("hapflk", "unstd_ihs", "win51_H12", "win51_pi","win51_TD"))
dfv2 <- f[,col2]
f3 <- Getdf(dfv2)
head(f3)
quartz()
ComparePlot(f3[,6:11])
### run time for "hapflk", "unstd_ihs", "win51_H12"
# Calculating outlierliness based on FastPCS...
#    user  system elapsed
#   1.677   0.053   1.870
# Calculating outlierliness based on clustering (DmWR)...
#    user  system elapsed
#  73.233  50.469 290.918
# Calculating outlierliness based on Mahalanobis distance...
#    user  system elapsed
#   0.011   0.007   0.033
# Calculating outlierliness based on harmonic mean of euclidean distance...
#    user  system elapsed
#   5.757   0.094   6.049
# Calculating outlierliness based on kernel density and given bandwith...
#    user  system elapsed
# 301.383   5.526 335.076
# Calculating outlierliness based on euclidean distance to nearest neighbor ...
#    user  system elapsed
#   5.157   0.093   5.487

### run time for "hapflk", "unstd_ihs", "win51_H12", "win51_pi","win51_TD"
# Calculating outlierliness based on FastPCS...
# [1] "FastPCS has found n/2 observations on a subspace."
#    user  system elapsed
#   4.969   0.100   5.241
# Calculating outlierliness based on clustering (DmWR)...
#    user  system elapsed
#  61.964  34.788 184.963
# Calculating outlierliness based on Mahalanobis distance...
#    user  system elapsed
#   0.019   0.008   0.027
# Calculating outlierliness based on harmonic mean of euclidean distance...
#    user  system elapsed
#   7.714   0.129   8.093
# Calculating outlierliness based on kernel density and given bandwith...
#  user  system elapsed
#   330.599   5.419 380.661
# Calculating outlierliness based on euclidean distance to nearest neighbor ...
#    user  system elapsed
#   7.431   0.087   7.625
