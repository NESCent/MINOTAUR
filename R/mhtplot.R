#'  L Wang
#'  March 19, 2015
#'  Manhattan Plot for Genome wide data
#'  @param mydata is a dataframe containing SNPs in rows and statistics in columns
#'  @param statistic.plot is the column name which value should be plotted (y axis)
#'  @param pval the column names for detecting the outliers
#'  @author Liuyang Wang
#'  @examples
#'  mhtplot(mydata=mytoy,statistic.plot="Trait1_Beta", pval="Trait1_P",Chr="Chr", BP="BP",pcut.outlier=0.001)
#'  
#'  @export


mhtplot<-function(mydata=mytoy, statistic.plot="Trait1_Beta", pval="Trait1_P",Chr="Chr", BP="BP", ylim.max=10,ylim.min=-10,colfig=NULL,titlemain=NULL, pcut.outlier=1e-4){
  ### Manhattan Plot. 
  ### The Yaxis = Beta value (slope) from GWAS, the color = significant degree (p value)
  ### Input is data.frame of R
  ### chr = "Chr", indicate the column name for Chromosome in mytoy (dataset).
	#mydata= mytoy;  pval = "Trait1_P"; BP= "BP" ;colfig=NULL
	#staistic.plot="Trait1_Beta"; Chr="Chr"; ylim.max=10; ylim.min=-10;titlemain=NULL; pcut.outlier=1e-2
	  
  if("scales" %in% rownames(installed.packages()) == FALSE) {
    install.packages("scales")
  }
  
  pvidx = match(pval, names(mydata))
  chridx = match(Chr,names(mydata))
  betaidx = match(statistic.plot, names(mydata))
  BPidx = match(BP, names(mydata))

  mynewtoy <- split(mydata, mydata[,Chr])
      
  number_snp <- dim(pval)[1]

  #p.max <- floor(max(-log10(mydata[,pvidx]),na.rm=T)+1)
  ylim.max <- floor(max(mydata[,betaidx], na.rm=T) + 1)
  ylim.min <- floor(min(mydata[,betaidx], na.rm=T) - 1)
  
  chrs.max <- lapply(sapply(mynewtoy,'[','BP'),max)
  x.total <- cumsum(as.numeric(unlist(chrs.max)))
  
  x.axis.scale<-300/max(x.total)
  
  if(is.null(colfig)) {
    colfig <- rep(c(alpha("grey",0.6),alpha("black",0.3)),round(length(mynewtoy)/2,0))
  } else{
    colfig <- rep(c(alpha(colfig[1],0.6),alpha(colfig[2],0.3)),round(length(mynewtoy)/2,0))
  }
  
  #col.outlier <- rep(c("blue","red"),round(length(mynewtoy)/2,0))
  
  plot(x=1:300,type="n",ylim=c(ylim.min,ylim.max),xlab="Chromosomes",ylab="Beta",main=titlemain,axes=F)
  abline(h=0,col=gray(0.5),lty="dashed")
  for (i in 1:length(x.total)){
    if(i==1){
      x.axis=mynewtoy[[i]][,BPidx] * x.axis.scale
    } else{
      x.axis=(x.total[i-1] + mynewtoy[[i]][,BPidx])*x.axis.scale
    }
    #mycols = alpha(colfig[i],(-log10(mynewtoy[[i]][,pvidx]))/p.max)
    mycols = c(alpha("black",0.4), alpha("black",0.2))
    points(x=x.axis,y=mynewtoy[[i]][,betaidx],pch=18,cex=0.6,col=colfig[i])	

    data.outlier <- which(mynewtoy[[i]][,pval] < pcut.outlier)  
    points(x=x.axis[data.outlier], y=mynewtoy[[i]][data.outlier,betaidx], pch=18,cex=0.6,col="red")
    #rug(x.axis,ticksize = 0.01, side = 1, lwd = 0.5,col=gray(0.6))  
  }
  
  x.total2<-c(0,x.total)
  axis(1,at=x.axis.scale*x.total2,labels=F)
  axis(1,at=x.axis.scale * x.total2[-1]-diff(x.axis.scale*x.total2)/2,labels=c(1:length(x.total)),cex=0.1,tick=F,cex.axis=0.8)
  axis(2,at=c(seq(ylim.min,0,2),seq(0,ylim.max,2)),label=T)
}

