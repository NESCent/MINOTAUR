

## FastPCS ##
library(FastPCS)
foo <- read.table("C:/Cait 2012/Work/Hackathon- PopGen/MANIPULATE/OneRefSim.txt", header=TRUE)
temp <- foo[,c(1, 11,12,16)]

row.names(temp) <- temp[,1]
temp <- temp[,c(2:ncol(temp))]
temp <- as.matrix(temp)
class(temp) <- "numeric"
temp <- na.omit(temp)

out <- FastPCS(temp, nSamp=NULL, alpha=0.5, seed=1)

out$distance ## the variable that gets plotted
out$best ## the indices of the data (ie. the indices of the SNPs) sorted by outlyingness

plot(out$distance)