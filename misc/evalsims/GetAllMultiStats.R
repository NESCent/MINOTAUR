
#'  KE Lotterhos
#'  Feb 18, 2016
#'  Get a dataframe with all multivariate stats
#'  @param dfv2 is a dataframe containing observations in rows and statistics in columns
#'  @param colnums is a vector of column numbers on which to compute the multivar stat
#'  @param alpha is for the FastPCS function
#'  @author KE Lotterhos

# dfv <- read.table("data/OneRefSim.txt", header=TRUE)
# colnums <- c(10,11,12,15)
source("misc/evalsims/distanceFunctionsOther.R")

Getdf <- function(dfv, colnums=1:ncol(dfv)){
  ### Check for duplicated rows and abort
  if (any(duplicated(dfv))) {
    writeLines("Error: Your data frame has duplicated rows")
    dfv[duplicated(dfv),]
    break()
  }
  
  dfv2 <- dfv[,colnums]
  rows.keep <- rep(TRUE, nrow(dfv2))
  ### Remove NAs
  if (any(is.na(dfv2))) {
    rows.keep <- !is.na(rowSums(dfv2))
    dfv2 <- dfv2[rows.keep,]
    writeLines(c("Rows with NAs were removed.  The data now has this many rows:", nrow(dfv2)))
  }



  writeLines("Calculating outlierliness based on FastPCS...")
    x<- system.time({
      tx <- try(pcs<-FastPCS.out(dfv2))
      if("try-error" %in% class(tx)){
        pcs <- NA
      }
    })
    print(x)

  writeLines("Calculating outlierliness based on clustering (DmWR)...")
     x<- system.time({
       tx <- try(Hcd <- hclust.ranking(dfv2))
       if("try-error" %in% class(tx)){
         Hcd <- NA
       }
     })
     print(x)

  writeLines("Calculating outlierliness based on Mahalanobis distance...")
    x<- system.time({
      tx <- try(Md <- Mahalanobis(dfv2))
      if("try-error" %in% class(tx)){
        Md <- NA}
      })
    print(x)

  writeLines("Calculating outlierliness based on harmonic mean of euclidean distance...")
    x<- system.time({
      tx <- try(Hd <- harmonicDist(dfv2))
      if("try-error" %in% class(tx)){
        Hd <- NA}
      })
    print(x)

  writeLines("Calculating outlierliness based on kernel density and given bandwith...")
    x<- system.time({
      bw <- c(seq(0.01,0.1,by=0.01),seq(0.2,1,by=0.1))
      #plot(bw, Kd.ML)
     tx <- try({
      Kd.ML <- kernelLogLike(dfv2, bandwidth=bw)
      bw.best <- bw[which(Kd.ML==max(Kd.ML))[1]]
      Kd <- kernelDist(dfv2, bandwidth=bw.best)
      })
     if("try-error" %in% class(tx)){Kd <- NA}
    })
    print(x)

  writeLines("Calculating outlierliness based on euclidean distance to nearest neighbor ...")
    x<- system.time({
      tx <- try(Nd <- neighborDist(dfv2))
      if("try-error" %in% class(tx)){
        Nd <- NA}
      })
    print(x)

  dfv$pcs[rows.keep] <- pcs
  dfv$Hcd[rows.keep] <- Hcd
  dfv$Md[rows.keep] <- Md
  dfv$Hd[rows.keep] <- Hd
  dfv$Kd[rows.keep] <- Kd
  dfv$Nd[rows.keep] <- Nd
  return(dfv)

}
