
#'  KE Lotterhos
#'  Feb 18, 2016
#'  Get a dataframe with all multivariate stats
#'  @param dfv2 is a dataframe containing observations in rows and statistics in columns
#'  @param colnums is a vector of column numbers on which to compute the multivar stat
#'  @param alpha is for the FastPCS function
#'  @author KE Lotterhos


Getdf <- function(dfv, colnums, n.sd=1.5, alpha=0.5, whichfun = "all"){


  ### Remove NAs
  if (any(is.na(dfv2[,colnums]))) {
    rows.keep <- apply(!is.na(dfv2[,colnums]), 1, any)
    dfv2 <- dfv2[rows.keep,]
    writeLines("Rows with NAs were removed")
  }

  ### Check for duplicated rows and abort
  if (any(duplicated(dfv2))) {
    writeLines("Error: Your data frame has duplicated rows")
    dfv2[duplicated(dfv2),]
    break()
  }

  writeLines("Calculating outlierliness based on FastPCS...")
    x<- system.time({
      tx <- try(pcs<-FastPCS.out(dfv2, colnums, alpha))
      if("try-error" %in% class(tx)){
        pcs <- list(D.pcs = NA, D.pcs.rank = NA, minus.log.emp.p = NA)
      }
    })
    print(x)

  writeLines("Calculating outlierliness based on clustering (outlier.ranking in DmWR)...")
     x<- system.time({
       tx <- try(Hcd <- hclust.ranking(dfv2, colnums))
       if("try-error" %in% class(tx)){
         Hcd <- list(empDens = NA, Dk.rank = NA, minus.log.emp.p = NA)
       }
     })
     print(x)

  writeLines("Calculating outlierliness based on Mahalanobis distance...")
    x<- system.time({
      tx <- try(Md <- Mahalanobis(dfv2, colnums))
      if("try-error" %in% class(tx)){Md <- list(Dm = NA, Dm.rank = NA, minus.log.emp.p = NA)}
      })
    print(x)

  writeLines("Calculating outlierliness based on harmonic mean of euclidean distance...")
    x<- system.time({
      tx <- try(Hd <- harmonicDist(dfv2[colnums]))
      if("try-error" %in% class(tx)){Hd <- list(empDens = NA, Dk.rank = NA, minus.log.emp.p = NA)}
      })
    print(x)

  writeLines("Calculating outlierliness based on kernel density and given bandwith...")
    x<- system.time({
      tx <- try(Kd <- kernelDist(dfv2, colnums, n.sd))
      if("try-error" %in% class(tx)){Kd <- list(empDens = NA, Dk.rank = NA, minus.log.emp.p = NA)}
      })
    print(x)

  writeLines("Calculating outlierliness based on kernel density and maximum likelihood...")
    x<- system.time({
      tx <- try(Kd.ML <- kernelLogLike(dfv2, colnums))
      if("try-error" %in% class(tx)){Kd.ML <- NA}
    })
    print(x)


  return(data.frame(dfv2, Md=Md$Dm, Md.rank=Md$Dm.rank, Md.mlp=Md$minus.log.emp.p,
                    Kd=Kd$empDens, Kd.rank=Kd$Dk.rank, Kd.mlp=Kd$minus.log.emp.p,
                    #Hcd.rank=Hcd$h.rank, Hcd.mlp=Hcd$minus.log.p,
                    Kd.ML.mll=Kd.ML$Kd.ML.mll, Kd.ML.rank= Kd.ML$Kd.ML.rank, Kd.ML.mlp=Kd.ML$minus.log.emp.p,
                    pcs.d = pcs$D.pcs, pcs.rank=pcs$D.pcs.rank, pcs.mlp=pcs$minus.log.emp.p)
         )
}
