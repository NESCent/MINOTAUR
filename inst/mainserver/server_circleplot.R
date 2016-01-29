#############################
## circular Manhattan plot ##
#############################


#######################
## .getCircleMHTPlot ##
#######################
.getCircleMHTPlot <- function(mainData){
  
  xchr = input$choose_xaxis_chrs
  xcood = input$choose_xaxis_coods
  colnam1 = input$Circle_y1
  colnam2 = input$Circle_y2
  logV1 = input$logV1Checkbox
  logV2 = input$logV2Checkbox
  poutlier = as.numeric(input$pcut)
  
  circleplots <- NULL
  if(!is.null(mainData)){
    if(!is.null(colnam1) && !is.null(colnam2)){
      y1_negNo <- length(which(mainData[, colnam1] <0))
      y2_negNo <- length(which(mainData[, colnam2] <0))
      
      
      ## NEED TO FIX AND REMOVE COMMENT MARKS FROM NEXT LINES OF CODE---WAS CAUSING AN ERROR SO I REMOVED IT JUST FOR THE MOMENT!!!!!!!!!!!!!!!
      ## WHERE IS THIS "logy" DEFINED???????????????????????????????????????????????????????????????????????????????????
      ## DOES IT NEED TO BE DEFINED ABOVE WITHIN THE FUNCTION OR
      ## ADDED AS AN ARGUMENT AT THE BEGINNING OF THE .getCircleMHTPlot FUNCTION????????
      
      #if( logy != "none"){
        #         if(y1_negNo > 0 || y_negNo > 0) {   
        #           stop("The y-axis variable contains negative values, 
        #                  can't be log-tranformed")
        #         }
      #}
      
      circleplots <- .circosmht(mydata=mainData,
                               Chr = xchr, BP=xcood, 
                               traitsname = c(colnam1, colnam2), 
                               pcut.outlier= poutlier, 
                               logV1 = logV1, logV2 = logV2)
    } 
  } 
  circleplots
} # end .getCircleMHTPlot

################
## .linkcreat ##
################
.linkcreat <- function(dat = seg.value, traitid = NULL, 
                      pvalid = NULL, pcut.outlier=0.002){
  ## a function for create link line for each trait
  #dat = seg.value; traitid = 3; pvalid = 4;pcut.outlier=0.001
  dat.outlier = dat[which(dat[, pvalid] < pcut.outlier),c("seg.name","seg.no")]
  snpbp.pair <- t(combn(dat.outlier[,2],2))
  snpchr.pair <- t(combn(dat.outlier[,1],2))
  
  link.outlier.v <- data.frame(seg1 = snpchr.pair[,1],
                               pos1=snpbp.pair[,1],
                               name1=paste("n",1:dim(snpchr.pair)[1],sep=""),
                               seg2=snpchr.pair[,2],pos2=snpbp.pair[,2])
  return(link.outlier.v)
} # end .linkcreat

###############
## .circosmht ##
###############
.circosmht <- function(mydata=mytoys,
                      BP= "BP", Chr="Chr", 
                      traitsname = c("Trait1_Beta","Trait2_Beta"), 
                      logV1 = FALSE, logV2 =FALSE , 
                      trait.pvalnam = c("Trait1_P","Trait2_P"), 
                      pcut.outlier=0.002){
  
  require(OmicCircos)
  
  seg.file <- data.frame(seg.name=mydata[,Chr], 
                         seg.Start=mydata[,BP], 
                         seg.End=mydata[,BP]+1, 
                         the.v="NA", NO="NA")
  #seg.value <- data.frame(seg.name=mydata[,Chr], 
  #                 seg.po=mydata[,BP],name1=mydata[,betaidx])
  seg.value = subset(mydata, select=-SNP)
  
  traitidxlist = match(traitsname, names(seg.value))  
  trait.pidxlist = match(trait.pvalnam, names(seg.value))
  chridx = match(Chr,names(seg.value))
  BPidx = match(BP, names(seg.value))

  if(!is.null(logV1)){
    if(length(logV1) > 1){
      logV1 = FALSE
    } else{
      if(logV1 == "-log2"){
        seg.value[,traitidxlist[1]] = -log2(abs(seg.value[,traitidxlist[1]]))
      } else if (logV1 == "-log10"){
        seg.value[,traitidxlist[1]] = -log10(abs(seg.value[,traitidxlist[1]]))
      } else{
        seg.value[,traitidxlist[1]] = seg.value[,traitidxlist[1]]
      }
    }
  }  
  if(!is.null(logV2)){
    if(length(logV2) > 1){
      logV2 = FALSE
    } else{
      if(logV2 == "log2"){
        seg.value[,traitidxlist[2]] = -log2(abs(seg.value[,traitidxlist[2]]))
      } else if (logV2 == "log10"){
        seg.value[,traitidxlist[2]] = -log10(abs(seg.value[,traitidxlist[2]]))
      } else{
        seg.value[,traitidxlist[2]] = seg.value[,traitidxlist[2]]
      }
    }
  }
  
  
  names(seg.value)[chridx] <- "seg.name"
  names(seg.value)[BPidx] <- "seg.no"
  seg.number <- length(unique(mydata[,Chr]))  
  seg.name <- sort(unique(mydata[,Chr]))
  db<-segAnglePo(seg.file,seg=seg.name);
  
  #colors<-rainbow(seg.number,alpha=0.5);
  colors <- brewer.pal(9, "Set1")

  par(mar=c(2,2,2,2));
  par(cex.axis=1, cex.lab=1, cex.main=1.2, cex.sub=1);
  plot(c(1,800),c(1,800),type="n",axes=F,xlab="",ylab="",main="");
  circos(R=400,type="chr",cir=db, 
         col=rep(alpha(colors,0.6),
                 length.out=seg.number), 
         print.chr.lab=T, W=40,scale=T);

  for (i in 1:length(traitidxlist)){
     tmpcolor = alpha(colors[i], 0.3)
     outlier.link = .linkcreat(dat=seg.value, 
                              traitid=traitidxlist[i], 
                              pvalid=trait.pidxlist[i], 
                              pcut.outlier=pcut.outlier)
     circos(R= 120 + (i-1) * 100, cir=db, 
            W= 180, mapping=seg.value, 
            col.v=traitidxlist[i],type="s", 
            B=F, col=tmpcolor,lwd=0.15, scale=T);
     circos(R=100, cir=db, W=100,
            mapping=outlier.link, 
            type="link",lwd=0.2,col= tmpcolor);
  }
 
} # end .circosmht

