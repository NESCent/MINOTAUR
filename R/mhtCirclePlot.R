#'  L Wang
#'  March 19, 2015
#'  Circle Manhattan Plot for Genome wide data. Each circles from outer to inner represent a trait. The inner links represnt outlier loci for each trait.
#'  @param mydata is a dataframe containing SNPs in rows and statistics in columns
#'  @param BP is column name for SNP position
#'  @param Chr is column name for Chromosome
#'  @param traitsname is a list, contains column names of the statistic values you want to plot
#'  @param trait.pvalnam is a list, contains column names of p value of statistic values. The order should match to traitsname.
#'  @param pcut.outlier is a value for pvalue cutoff
#'  @author Liuyang Wang
#'  @examples
#'  circosmht(mydata=mytoys, BP= "BP", Chr="Chr", traitsname = c("Trait1_Beta","Trait2_Beta","Trait3_Beta"),  trait.pvalnam = c("Trait1_P","Trait2_P","Trait3_P"), pcut.outlier=0.002)
#'  
#'  
#'  @export


linkcreat <- function(dat = seg.value, traitid = NULL, pvalid = NULL, pcut.outlier=0.002){
  ## a function for create link line for each trait
  #dat = seg.value; traitid = 3; pvalid = 4;pcut.outlier=0.001
  dat.outlier = dat[which(dat[, pvalid] < pcut.outlier),c("seg.name","seg.no")]
  snpbp.pair <- t(combn(dat.outlier[,2],2))
  snpchr.pair <- t(combn(dat.outlier[,1],2))
  
  link.outlier.v <- data.frame(seg1 = snpchr.pair[,1],pos1=snpbp.pair[,1],name1=paste("n",1:dim(snpchr.pair)[1],sep=""),seg2=snpchr.pair[,2],pos2=snpbp.pair[,2])
  return(link.outlier.v)
}

loadpackages <- function(){
  if("scales" %in% rownames(installed.packages()) == FALSE) {
    install.packages("scales");
  }
  if("RColorBrewer" %in% rownames(installed.packages()) == FALSE) {
    install.packages("RcolorBrewer")
  }
  if("OmicCircos" %in% rownames(installed.packages()) == FALSE) {
    source("http://bioconductor.org/biocLite.R");
    biocLite("OmicCircos");
  }
  suppressMessages(require(RColorBrewer));
  suppressMessages(require(scales));
  suppressMessages(require(OmicCircos));
}

circosmht <- function(mydata=mytoys,BP= "BP", Chr="Chr", traitsname = c("Trait1_Beta","Trait2_Beta","Trait3_Beta"), trait.pvalnam = c("Trait1_P","Trait2_P","Trait3_P"), pcut.outlier=0.002){

  loadpackages()
  mydata = mytoys
  seg.file <- data.frame(seg.name=mydata[,Chr], seg.Start=mydata[,BP], seg.End=mydata[,BP]+1, the.v="NA", NO="NA")
  #seg.value <- data.frame(seg.name=mydata[,Chr], seg.po=mydata[,BP],name1=mydata[,betaidx])
  seg.value = subset(mydata, select=-SNP)

  traitidxlist = match(traitsname, names(seg.value))  
  trait.pidxlist = match(trait.pvalnam, names(seg.value))
  
  circlewidth <- 60
  if(length(traitidxlist) > 6){
    circlewidth <- round(300/length(traitidxlist),0)
  }
  
  chridx = match(Chr,names(seg.value))
  BPidx = match(BP, names(seg.value))
  #pvidx = match(trait.pvalnam, names(seg.value))
  #betaidx = match(statistic.plot, names(seg.value))
  
  names(seg.value)[chridx] <- "seg.name"
  names(seg.value)[BPidx] <- "seg.no"
  seg.number <- length(unique(mydata[,Chr]))  
  seg.name <- sort(unique(mydata[,Chr]))
  db<-segAnglePo(seg.file,seg=seg.name);
  
  #colors<-rainbow(seg.number,alpha=0.5);
  colors <- brewer.pal(9, "Set1")
  
  linkpos <- 400 - length(traitidxlist) * circlewidth - 20
  #linkpos <- ifelse(linkpos > 200, 200, 100)
  if (linkpos > 200){
    linkpos <- 200
  } else if(linkpos > 100){
    linkpos <- 100
  }
  
  par(mar=c(2,2,2,2));
  plot(c(1,800),c(1,800),type="n",axes=F,xlab="",ylab="",main="");
  circos(R=400,type="chr",cir=db, col=rep(alpha(colors,0.6),length.out=seg.number), print.chr.lab=T, W=40,scale=T);
  for (i in 1:length(traitidxlist)){
    tmpcolor = alpha(colors[i], 0.3)
    outlier.link = linkcreat(dat=seg.value, traitid=traitidxlist[i], pvalid=trait.pidxlist[i], pcut.outlier=pcut.outlier)
    circos(R=400 - i* circlewidth,cir=db,W=circlewidth,mapping=seg.value, col.v=traitidxlist[i],type="s",B=F,col=tmpcolor,lwd=0.15, scale=T);
    circos(R=linkpos,cir=db,W=50,mapping=outlier.link, type="link",lwd=0.2,col= tmpcolor);
  }
  
}


