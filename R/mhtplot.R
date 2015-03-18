

mymhtPlot<-function(mytoy=mytoy, chr="Chr",pval="Trait1_P",Chr="Chr", BP="BP", ylim.up=10,ylim.dn=-10,colfig=NULL,titlemain=NULL){

  require(scales)
  pvidx = match(pval, names(mytoy))
  chridx = match("Chr",names(mytoy))
  betaidx = match("Trait1_Beta", names(mytoy))
  BPidx = match(BP, names(mytoy))

  mynewtoy <- split(mytoy, mytoy[,chr])
      
  number_snp <- dim(pval)[1]

  p.max <- floor(max(-log10(mytoy[,pvidx]),na.rm=T)+1)
  ylim.up <- floor(max(mytoy[,betaidx], na.rm=T) + 1)
  ylim.dn <- floor(min(mytoy[,betaidx], na.rm=T) - 1)
  
  chrs.max <- lapply(sapply(mynewtoy,'[','BP'),max)
  x.total <- cumsum(as.numeric(unlist(chrs.max)))
  
  x.axis.scale<-300/max(x.total)
  
  if(is.null(colfig)) 
    colfig <- rep(c("red","blue"),round(length(mynewtoy)/2,0))
  
  plot(x=1:300,type="n",ylim=c(ylim.dn,ylim.up),xlab="Chromosomes",ylab="Beta",main=titlemain,axes=F)
  abline(h=0,col=gray(0.5),lty="dashed")
  for (i in 1:length(x.total)){
    if(i==1){
      x.axis=mynewtoy[[i]][,BPidx] * x.axis.scale
    } else{
      x.axis=(x.total[i-1] + mynewtoy[[i]][,BPidx])*x.axis.scale
    }
    mycols = alpha(colfig[i],(-log10(mynewtoy[[i]][,pvidx]))/p.max)
    points(x=x.axis,y=mynewtoy[[i]][,betaidx],pch=18,cex=0.6,col=mycols)	
    #rug(x.axis,ticksize = 0.01, side = 1, lwd = 0.5,col=gray(0.6))  
  }
  
  x.total2<-c(0,x.total)
  axis(1,at=x.axis.scale*x.total2,labels=F)
  axis(1,at=x.axis.scale * x.total2[-1]-diff(x.axis.scale*x.total2)/2,labels=c(1:length(x.total)),cex=0.1,tick=F,cex.axis=0.8)
  axis(2,at=c(seq(ylim.dn,0,2),seq(0,ylim.up,2)),label=T)
}

