########### RRCOV #############
install.packages("rrcov")
library(rrcov)
### COV MVE calculates covariance - not outlier
### Maryo - showes how a robust estimate of CovMcd can recover true correlation when outliers are present
### PcaHubert-class - a PCA robust to outliers
data(maryo)
head(maryo)
par(mfrow=c(1,1))
plot(maryo[,1], maryo[,2])

########### MVOUTLIER #############
install.packages("mvoutlier")
library(mvoutlier)

########### MASS #############
# cov.rob Compute a multivariate location and scale estimate with a 
# high breakdown point – this can be thought of as estimating the 
# mean and covariance of the good part of the data. cov.mve and cov.mcd are 
# compatibility wrappers.

### other packages
# compute normal and robust PCAs
