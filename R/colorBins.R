 bins <- seq(min,max, length.out = 100)
  #col <- grey(seq(0,0.9,length.out=99))
  col <- heat.colors(99)
  He.col <- rep(NA, nloc)
  for (i in 1:nloc){
    He.col[i] <- col[max(which(He[i]>=bins))]
  }