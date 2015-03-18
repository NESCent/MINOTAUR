
x <- rgamma(5000, shape=10, rate=2)
y <- 1/x + rnorm(5000, sd=0.5) + runif(5000)

x2 <- c(x,y, 6)
y2 <- c(y,x, 6)

plot(x2, y2)
