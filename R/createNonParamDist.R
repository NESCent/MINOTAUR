
x <- rgamma(5000, shape=10, rate=2)
y <- 1/x + rnorm(5000, sd=0.5) + runif(5000)

x2 <- c(x,y, 6)
y2 <- c(y,x, 6)

plot(x2, y2, col=c(rep(1,10000),3), pch=19)
write.table(data.frame(x2,y2),"inst/misc/NonParamEx.txt", row.names=FALSE)
