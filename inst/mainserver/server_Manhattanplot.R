
## linear Manhattan plot
output$linearMH_y1Selection <- renderUI({
    selectizeInput('choose_y1_plot','Select y-axis variable',choices=c(names(rv$subData)), selected = "Trait1_Beta" )
  })

output$linearMH_p2Selection <- renderUI({
  selectizeInput('choose_pval','Mark outliers by second variable (usually p value)',choices=c(names(rv$subData)), selected = "Trait1_P"  )
})

# linenar Manhattan Plot
output$LinearMHTplot <- renderPlot({
    yselect = input$choose_y1_plot
    logy = input$logy1Checkbox
    pselect = input$choose_pval
    #logp = input$logCheckbox
    poutlier = as.numeric(input$linearmhtpcut)

    mhtplots <- NULL
    if(!is.null(rv$subData)){
      if(!is.null(yselect) && !is.null(pselect)){
        mhtplots <- mhtplot(mydata=rv$subData, ycolnam=yselect, pcolnam=pselect, pcut.outlier= poutlier, logY=logy)
      } 
    } 
    mhtplots

    yname = input$yaxis
})


mhtplot<-function(mydata=mytoy, ycolnam="Trait1_Beta", pcolnam="Trait1_P",Chr="Chr", BP="BP", ylim.max=10,ylim.min=-10,colfig=NULL,titlemain=NULL,logY=FALSE, pcut.outlier=1e-4){
  ### Manhattan Plot. 
  ### The Yaxis = Beta value (slope) from GWAS, the color = significant degree (p value)
  ### Input is data.frame of R
  ### chr = "Chr", indicate the column name for Chromosome in mytoy (dataset).
  #mydata= mytoy;  pval = "Trait1_P"; BP= "BP" ;colfig=NULL
  #staistic.plot="Trait1_Beta"; Chr="Chr"; ylim.max=10; ylim.min=-10;titlemain=NULL; pcut.outlier=1e-2

  #pvidx = 5;  betaidx = 4;

  if(!is.null(ycolnam)){
    betaidx = match(ycolnam, names(mydata))
  } 
  
  if(!is.null(pcolnam)){
    pvidx = match(pcolnam, names(mydata))
  } 
  
  chridx = match(Chr,names(mydata))
  BPidx = match(BP, names(mydata))
  
  if(is.na(chridx)){
    print(" 'Chr' is not in the column names of input data ")
  }
  if(is.na(BPidx)){
    print(" 'BP' is not in the column names of input data ")
  }
  
  if(!is.null(logY)){
    if(length(logY) > 1){
      logV1 = FALSE
    } else{
      if(logY == "log2"){
        mydata[,betaidx] = -log2(abs(mydata[,betaidx]))
        ycolnam = paste(ycolnam, " log2(X)")
        pcut.outlier = -log2(pcut.outlier)
      } else if (logY == "log10"){
        mydata[,betaidx] = -log10(abs(mydata[,betaidx]))
        ycolnam = paste(ycolnam, " log10(X)")
        pcut.outlier = -log10(pcut.outlier)
      } else{
        mydata[,betaidx] = mydata[,betaidx]
        ycolnam = ycolnam
        pcut.outlier = pcut.outlier
      }
    }
  }
  
  mynewtoy <- split(mydata, mydata[,Chr])
  number_snp <- dim(mydata)[1]
  
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
  
  plot(x=1:300,type="n",ylim=c(ylim.min,ylim.max),xlab="Chromosomes",ylab=ycolnam ,main=titlemain,axes=F)
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
    
    data.outlier <- which(mynewtoy[[i]][,pvidx] < pcut.outlier)      
    if(logY %in% c("log2","log10")){
#       if(logY == "log2"){
#         mynewtoy[[i]][,pvidx] = -log2(mynewtoy[[i]][,pvidx])
#         pcut.outlier = -log2(pcut.outlier)
#       } else if (logY == "log10"){
#         mynewtoy[[i]][,pvidx] = -log10(mynewtoy[[i]][,pvidx])
#         pcut.outlier = -log10(pcut.outlier)       
#       }
      data.outlier <- which(mynewtoy[[i]][,pvidx] > pcut.outlier)  
    }
    
    points(x=x.axis[data.outlier], y=mynewtoy[[i]][data.outlier,betaidx], pch=18,cex=0.6*1.5,col="red")
    #rug(x.axis,ticksize = 0.01, side = 1, lwd = 0.5,col=gray(0.6))  
  }
  
  x.total2<-c(0,x.total)
  axis(1,at=x.axis.scale*x.total2,labels=F)
  axis(1,at=x.axis.scale * x.total2[-1]-diff(x.axis.scale*x.total2)/2,labels=c(1:length(x.total)),cex=0.1,tick=F,cex.axis=0.8)
  axis(2,at=c(seq(ylim.min,0,2),seq(0,ylim.max,2)),label=T)
}