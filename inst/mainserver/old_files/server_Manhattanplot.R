
###########################
## Linear Manhattan Plot ##
###########################

#######################
## .getLinearMHTPlot ##
#######################
.getLinearMHTPlot <- function(mainData, input){
  xchr = input$choose_xaxis_chr
  xcood = input$choose_xaxis_cood
  yselect = input$choose_y1_plot
  logy = input$logy1Checkbox
  pselect = input$choose_pval
  poutlier = as.numeric(input$linearmhtpcut)
  flipYaxis = input$flipY
  n_bins <- as.numeric(as.character(input$linearmht_nbins))

  mhtplots <- NULL
  if(!is.null(mainData)){
    if(!is.null(yselect) && !is.null(pselect)){
      mhtplots <- .mhtplot(mydata=mainData, Chr=xchr, BP = xcood, ycolnam=yselect, pcolnam=pselect,
                           pcut.outlier= poutlier, logY=logy, nbins=n_bins, flipY=flipYaxis)
    }
  }
  mhtplots
} # end .getLinearMHTPlot


##############
## .mhtplot ##
##############
.mhtplot <- function(mydata=mytoy, Chr="Chr", BP="BP",
                  ycolnam="Trait1_Beta", pcolnam="Trait1_P",
                  ylim.max=10,ylim.min=-10,
                  colfig=NULL,titlemain=NULL,logY=FALSE,
                  nbins=100, pcut.outlier=1e-4, flipY="No"){
  ### Manhattan Plot.

  if(!is.null(ycolnam)){    betaidx = match(ycolnam, names(mydata))  }
  if(!is.null(pcolnam)){    pvidx = match(pcolnam, names(mydata))  }

  chridx = match(Chr,names(mydata));  BPidx = match(BP, names(mydata))

  if(is.na(chridx)){  print(" 'Chr' is not in the column names of input data ")  }
  if(is.na(BPidx)) {   print(" 'BP' is not in the column names of input data ")  }

  data.outlier <- which(mydata[,pvidx] < pcut.outlier)

  if(logY %in% c("log2", "log10")){
    if(length(which(mydata[,betaidx] < 0)) > 0) {stop("Selected Y-axis variable contains negative values, can't be log-transformed\n ")}
    logbase = ifelse(logY %in% "log2",2, 10)
    mydata[,betaidx] = log(abs(mydata[,betaidx]),logbase)
    ycolnam = paste(logY,"(", ycolnam,")",sep="")
  } else {
    mydata[,betaidx] = mydata[,betaidx]
    ycolnam = ycolnam
  }

  flipY_base = ifelse(flipY %in% "Yes", -1, 1)
  mydata[, betaidx] = mydata[,betaidx] * flipY_base

  mynewtoy <- split(mydata, mydata[,Chr])
  number_snp <- dim(mydata)[1]

  ylim.max <- floor(max(mydata[,betaidx], na.rm=T) + 1)
  ylim.min <- floor(min(mydata[,betaidx], na.rm=T) - 1)
  if(ylim.min > 0) { ylim.min = 0}
  if(ylim.max < 0) { ylim.max = 0}

  chrs.max <- lapply(sapply(mynewtoy,'[',BP),max)
  x.total <- cumsum(as.numeric(unlist(chrs.max)))
  x.axis.scale<-300/max(x.total)

  if(is.null(colfig)) {
    colfig <- rep(c(alpha("grey",0.6),alpha("black",0.3)),  round(length(mynewtoy)/2,0))
  } else{
    colfig <- rep(c(alpha(colfig[1],0.6),alpha(colfig[2],0.3)),   round(length(mynewtoy)/2,0))
  }

  plot(x=1:300,type="n",ylim=c(ylim.min,ylim.max), xlab="Chromosome",ylab=ycolnam ,main=titlemain,axes=F)
  abline(h=0,col=gray(0.5),lty="dashed")

  xaxis_all <- c();  yaxis_all <- c();  collib <- c()

  for (i in 1:length(x.total)){
    if(i==1){
      x.axis=mynewtoy[[i]][,BPidx] * x.axis.scale
    } else{
      x.axis=(x.total[i-1] + mynewtoy[[i]][,BPidx])*x.axis.scale
    }

    xaxis_all <- c(xaxis_all,x.axis)
    yaxis_all <- c(yaxis_all, mynewtoy[[i]][,betaidx])
  }

  dat4plots <- data.frame(xv = xaxis_all, yv=yaxis_all)
  dat4plot <- data.matrix(dat4plots[complete.cases(dat4plots),])

  x.axis.min <- min(dat4plot[,1], na.rm=T)
  x.axis.max <- max(dat4plot[,1], na.rm=T)
  y.axis.min <- min(dat4plot[,2], na.rm=T)
  y.axis.max <- max(dat4plot[,2], na.rm=T)

  datbin <- bin2(dat4plot, matrix(c(x.axis.min, x.axis.max, y.axis.min, y.axis.max),
                        2,2, byrow=TRUE),   nbin=c(nbins,nbins))

  datbin$nc[datbin$nc==0] = NA

  image.plot(seq(x.axis.min,x.axis.max,length.out = nbins),
             seq(y.axis.min, y.axis.max, length.out=nbins),
             datbin$nc, xlab="", ylab="", add=FALSE,
             col=grey.colors(60, 0.6,0), axes=FALSE)

  points(x=xaxis_all[data.outlier], y=yaxis_all[data.outlier], pch=18, cex=1,col="red")

  x.total2<-c(0,x.total)
  axis(1,at=x.axis.scale*x.total2,labels=F)
  axis(1,at=x.axis.scale * x.total2[-1]-diff(x.axis.scale*x.total2)/2, labels=c(1:length(x.total)),cex=0.1,tick=F,cex.axis=0.8)
  axis(2,at=axTicks(2),label=T)
} # end .mhtplot


################################
## .getManhattanPlotDataTable ##
################################
.getManhattanDataTable <- function(input, mainData){
  if(!is.null(mainData)){
    Pselect = input$choose_pval
    if(!is.null(Pselect)){
      colData = mainData[,names(mainData)== Pselect]
      cutoff <- as.numeric(input$linearmhtpcut)
      indexes <- which(colData < cutoff)
      df <- mainData[indexes,]
      ## run utils fn to show NAs in renderDataTable output
      .showNAs(df)
    }
  }
} # end .getManhattanPlotDataTable
