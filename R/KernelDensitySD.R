### KE Lotterhos
### March 8 2015

MulitD.EmpDens <- function(df.vars, n.sd=1.5){
  ### This function takes a dataframe of statistics where each row is a locus/observation 
  ### and each column is an observation statistic
  ### It breaks up multivariate space into n-dimensional chunks, 
  ### with the size of each chunk determined by the standard deviation 
  ### (n.sd) is the proportion of the standard deviation
  ### n.chunks is the number of chunks inside 1SD of each statistic
  ### Next the density of points inside each chunk is calculated, and 
  ### chunks are ranked according to their density.  Ranks are used to create an empirical p-value

  n.stat <- ncol(df.vars)
  nlocs <- nrow(df.vars)
  width <- rep(NA, n.stat) # store width of chunk in each variable
  df.vars.sorted <- df.vars
  
  for (i in 1:n.stat){
    width[i] <- sd(df.vars[,i])*n.sd
    df.vars.sorted[,i] <- sort(df.vars[,i])
  }
  
  empDens <- rep(NA, nrow(df.vars))
  temp.logic <- df.vars
  
  for (i in 1:nlocs){
    for (j in 1:n.stat){
        temp.logic[,j] <- ((df.vars.sorted[,j]<=(df.vars[i,j]+width[j])) & (df.vars.sorted[,j]>=(df.vars[i,j]-width[j])))
    }
    empDens[i] <- sum(rowSums(temp.logic)==n.stat)
  }
  
  return(list(empDens, emp.p=rank(empDens)/nlocs))
}