

###################
## UI FUNCTIONS! ##
###################


##################################
## MANHATTAN PLOT PAGE (linear) ##    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###
##################################

#######################
## .getMHTySelection ##
#######################
.getMHTySelection <- function(mainData){
  out <- NULL
  if(!is.null(mainData)){
    sel <- NULL
    ## get numeric variables
    numCols <- which(sapply(c(1:ncol(mainData)),
                            function(e) is.numeric(mainData[,e])))
    ## if possible, choose a non-integer numeric column
    intCols <- which(sapply(c(1:ncol(mainData)),
                            function(e) is.integer(mainData[,e])))
    if(length(setdiff(numCols, intCols)) > 0){
      numCols <- setdiff(numCols, intCols)
    }
    ## get selected
    if(length(numCols) > 0){
      sel <- names(mainData)[numCols[1]]
      if(length(numCols) >= 2){
        sel <- names(mainData)[numCols[2]]
      }
    }

    out <- selectInput(
      inputId = "choose_y1_plot",
      label = "Choose y-axis variable:",
      choices = names(mainData),
      selected = sel #may want to update with publishable dataset
    )
  }
  return(out)
} # end .getMHTySelection

##########################
## .getMHTxChrSelection ##
##########################
.getMHTxChrSelection <- function(mainData){
  out <- NULL
  if(!is.null(mainData)){
    sel <- NULL

    ## get (potentially-chromosome-type) variables ???????????????????????????????????????????????????????????
    sel <- names(mainData)[1]

    out <- selectInput(
      inputId = "choose_xaxis_chr",
      label = "Choose chromosomes variable:",
      choices = names(mainData),
      selected = sel #may want to update with publishable dataset
    )
  }
  return(out)
} # end .getMHTxChrSelection

##########################
## .getMHTxPosSelection ##
##########################
.getMHTxPosSelection <- function(mainData){
  out <- NULL
  if(!is.null(mainData)){
    sel <- NULL

    ## get (potentially-position-type) variables ???????????????????????????????????????????????????????????
    sel <- names(mainData)[3]

    out <- selectInput(
      inputId = "choose_xaxis_cood",
      label = "Choose position variable:",
      choices = names(mainData),
      selected = sel #may want to update with publishable dataset
    )
  }
  return(out)
} # end .getMHTxPosSelection


#######################
## .getMHTpSelection ##
#######################
.getMHTpSelection <- function(mainData){
  out <- NULL
  if(!is.null(mainData)){
    sel <- NULL

    ## get numeric variables
    numCols <- which(sapply(c(1:ncol(mainData)),
                            function(e) is.numeric(mainData[,e])))
    ## if possible, choose a non-integer numeric column
    intCols <- which(sapply(c(1:ncol(mainData)),
                            function(e) is.integer(mainData[,e])))
    if(length(setdiff(numCols, intCols)) > 0){
      numCols <- setdiff(numCols, intCols)
    }
    ## NOTE: for the moment, I'm just picking the 2nd numeric var (bc it happens to be )
    ## Trait1_P for the human GWAS ("largeData") example dataset....
    ## But THIS SHOULD PROBABLY BE CHANGED !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ## get selected
    if(length(numCols) > 0){
      sel <- names(mainData)[numCols[1]]
      if(length(numCols) >= 2){
        sel <- names(mainData)[numCols[2]]
      }
    }

    out <- selectInput(
      inputId = "choose_pval",
      label = "Mark outliers by second variable
              (usually p-value):",
      choices = names(mainData),
      selected = sel #may want to update with publishable dataset
    )
  }
  return(out)
} # end .getMHTpSelection


###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###

######################
## CIRCLE PLOT PAGE ##    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###
######################

########################
## .getOuterCircleVar ##
########################
.getOuterCircleVar <- function(mainData){

  if(!is.null(mainData)){
    ## get choices
    noms <- names(mainData)

    ## get numeric variables
    numCols <- which(sapply(c(1:ncol(mainData)),
                            function(e) is.numeric(mainData[,e])))
    ## if possible, choose a non-integer numeric column
    intCols <- which(sapply(c(1:ncol(mainData)),
                            function(e) is.integer(mainData[,e])))
    if(length(setdiff(numCols, intCols)) > 0){
      numCols <- setdiff(numCols, intCols)
    }
    ## get selected
    if(length(numCols) > 0){
      sel <- names(mainData)[numCols[1]]
    }

    ## get input ui element
    selectizeInput('Circle_y1','Choose a variable for inner circle:',
                   choices=noms, multiple=FALSE,
                   selected = sel)
  }
} # end .getOuterCircleVar


########################
## .getInnerCircleVar ##
########################
.getInnerCircleVar <- function(mainData){

  if(!is.null(mainData)){
    ## get choices
    noms <- names(mainData)

    ## get numeric variables
    numCols <- which(sapply(c(1:ncol(mainData)),
                            function(e) is.numeric(mainData[,e])))
    ## if possible, choose a non-integer numeric column
    intCols <- which(sapply(c(1:ncol(mainData)),
                            function(e) is.integer(mainData[,e])))
    if(length(setdiff(numCols, intCols)) > 0){
      numCols <- setdiff(numCols, intCols)
    }
    ## get selected
    if(length(numCols) > 0){
      sel <- names(mainData)[numCols[1]]
      if(length(numCols) >= 2){
        sel <- names(mainData)[numCols[2]]
      }
    }

    ## get input ui element
    selectizeInput('Circle_y2','Choose a variable for outer circle:',
                   choices=noms, multiple=FALSE,
                   selected = sel)
  }
} # end .getOuterCircleVar

#######################
## .getChromosomeVar ##
#######################
.getChromosomeVar <- function(mainData){
  if(!is.null(mainData)){
    ## get choices
    noms <- names(mainData)

    ## get (potentially-chromosome-type) variables ???????????????????????????????????????????????????????????
    sel <- names(mainData)[1]

    ## get input ui element
    selectizeInput('choose_xaxis_chrs', 'Choose Chromosomes:',
                   choices=noms, #multiple=FALSE,
                   selected = sel)
  }
} # .getChromosomeVar


##################
## .getCoordVar ##
##################
.getCoordVar <- function(mainData){
  if(!is.null(mainData)){
    ## get choices
    noms <- names(mainData)

    ## get (potentially-coordinate-type) variables ???????????????????????????????????????????????????????????
    sel <- names(mainData)[3]

    ## get input ui element
    selectizeInput('choose_xaxis_coods', 'Choose Coordinates:',
                   choices=noms, #multiple=FALSE,
                   selected = sel)
  }
} # .getChromosomeVar

###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###

######################
## SCATTERPLOT PAGE ##    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###
######################

# x = Chr, y = Trait1_P, outlier var = Trait2_Beta

####################
## .getxSelection ##
####################
.getxSelection <- function(mainData){
  out <- NULL
  if(!is.null(mainData)){

    sel <- NULL
    ## get numeric variables
    numCols <- which(sapply(c(1:ncol(mainData)),
                            function(e) is.numeric(mainData[,e])))
    ## if possible, choose a non-integer numeric column
    intCols <- which(sapply(c(1:ncol(mainData)),
                            function(e) is.integer(mainData[,e])))
    if(length(setdiff(numCols, intCols)) > 0){
      numCols <- setdiff(numCols, intCols)
    }
    ## get selected
    if(length(numCols) > 0){
      sel <- names(mainData)[numCols[1]]
    }
    out <- selectInput(
      inputId = "xSelection",
      label = "Choose x-axis data:",
      choices = names(mainData),
      selected = sel #may want to update with publishable dataset
    )
  }
  return(out)
} # end .getxSelection

####################
## .getySelection ##
####################
.getySelection <- function(mainData){
  out <- NULL
  if(!is.null(mainData)){
    sel <- NULL
    ## get numeric variables
    numCols <- which(sapply(c(1:ncol(mainData)),
                            function(e) is.numeric(mainData[,e])))
    ## if possible, choose a non-integer numeric column
    intCols <- which(sapply(c(1:ncol(mainData)),
                            function(e) is.integer(mainData[,e])))
    if(length(setdiff(numCols, intCols)) > 0){
      numCols <- setdiff(numCols, intCols)
    }
    ## get selected
    if(length(numCols) > 0){
      sel <- names(mainData)[numCols[1]]
      if(length(numCols) >= 2){
        sel <- names(mainData)[numCols[2]]
      }
    }

    out <- selectInput(
      inputId = "ySelection",
      label = "Choose y-axis data:",
      choices = names(mainData),
      selected = sel #may want to update with publishable dataset
    )
  }
  return(out)
} # end .getySelection


####################
## .getPSelection ##
####################
.getpSelection <- function(mainData){
  out <- NULL
  if(!is.null(mainData)){
    out <- selectInput(
      inputId = "pSelection",
      label = "Choose p value cutoff:",
      choices = names(mainData)
    )
  }
  return(out)
} # end .getxSelection

#########################
## .getColVarSelection ##
#########################
.getColVarSelection <- function(mainData){
  out <- NULL
  if(!is.null(mainData)){

    sel <- NULL
    ## get numeric variables
    numCols <- which(sapply(c(1:ncol(mainData)),
                            function(e) is.numeric(mainData[,e])))
    ## if possible, choose a non-integer numeric column
    intCols <- which(sapply(c(1:ncol(mainData)),
                            function(e) is.integer(mainData[,e])))
    if(length(setdiff(numCols, intCols)) > 0){
      numCols <- setdiff(numCols, intCols)
    }
    ## get selected
    if(length(numCols) > 0){
      sel <- names(mainData)[numCols[1]]
      if(length(numCols) >= 3){
        sel <- names(mainData)[numCols[3]]
      }
    }

    out <- selectInput(
      inputId = "colVarSelection",
      label = "Choose variable to highlight outliers for on plot:", ## THIS SHOULD PROBABLY BE RE-WORDED!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      choices = names(mainData),
      selected = sel #may want to update with publishable dataset
    )
  }
  return(out)
} # end .getColVarSelection

################
## .getColPal ##
################
.getColPal <- function(){
  out <- selectInput("colPal", "Choose a color pallette to use:",
                     c("SeaSun" = "seasun",
                       "Funky" = "funky",
                       "Spectral" = "spectral",
                       "Azur" = "azur",
                       "Wasp" = "wasp"
                     )
  )
  return(out)
} # end .getColPal

###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###

########################
## CLEAN-UP DATA PAGE ##    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###
########################

##############
## bobText1 ##
##############
# right-aligned numeric text box with adjustable width
# warning - this box can contain non-numeric strings.
# Check for these using suppressWarnings(!is.na(as.numeric(val)))
bobText1 <- function(inputId, label, value="",
                     size='10px', placeholder='',
                     style='text-align:right',...) {
  div(style="display:inline-block",
      tags$label(label, `for` = inputId),
      tags$input(id=inputId, type="text",
                 pattern='[0-9]{1,99}|.
                 [0-9]{1,99}|[0-9]{1,99}.
                 [0-9]{1,99}|inf|Inf|INF|-inf|-Inf|-INF',
                 value=value, size=size,
                 placeholder=placeholder, style=style,...)
  )
}

####################
## bobCloseButton ##
####################
# nice looking close button
bobCloseButton <- function(inputId) {
  div(
    class = "close",
    tags$button(id = inputId, type = "button",
                class = "btn action-button",
                style = "font-size:10px; width:25px;
                height:25px; text-align:center; line-height:10px",
                HTML("<i class='close'></i>&#10006"))
  )
} # end bobCloseButton

####################
## bobResetButton ##
####################
# nice looking reset button
bobResetButton <- function(inputId) {
  div(
    style="float:right",
    tags$button(id = inputId, type = "button",
                class = "btn action-button btn-primary",
                style = "font-size:15px; width:50px;
                line-height:12px; text-align:center",
                HTML("<i class='icon-star'></i>reset"))
  )
} # end bobResetButton

###################
## bobViolinPlot ##
###################
# violin-style plot
bobViolinPlot <- function(x,subMin=0,subMax=0) {
  out <- NULL

  x = x[!is.na(x)]
  par(mar=c(0,0,0,0))
  d = density(x, from=min(x), to=max(x))
  d$y = d$y/max(d$y)
  rangeMin = (min(x)+max(x))/2 - 1.5*(max(x)-min(x))/2
  rangeMax = (min(x)+max(x))/2 + 1.5*(max(x)-min(x))/2
  out <-
    plot(0,type='n',
         xlim=c(rangeMin,rangeMax),
         ylim=c(-1,1),axes=FALSE,
         xlab='',ylab='')

  polygon(c(d$x,rev(d$x)),
          c(d$y,-rev(d$y)),
          col=grey(0.9),border=NA)

  if (subMin!=subMax) {
    s1 = min(subMin,subMax)
    s2 = max(subMin,subMax)
    s1 = max(s1,min(x))
    s2 = min(s2,max(x))
    z1 = which(abs(d$x-s1)==min(abs(d$x-s1)))[1]
    z2 = which(abs(d$x-s2)==min(abs(d$x-s2)))[1]
    polygon(c(d$x[z1:z2],rev(d$x[z1:z2])),
            c(d$y[z1:z2],-rev(d$y[z1:z2])),
            col='#ff9000',border=NA)
    abline(v=c(d$x[z1],d$x[z2]),col='#ff9000',lty=2)
  }
  polygon(c(d$x,rev(d$x)),c(d$y,-rev(d$y)))
  text(min(x),0,signif(min(x),digits=3),pos=2)
  text(max(x),0,signif(max(x),digits=3),pos=4)

  return(out)
} # end bobViolinPlot

###################
## bobBarSubplot ##
###################
# bar sub plot
bobBarSubplot <- function(tab, selected, total, varClass="factor") {
  out <- NULL

  ## FACTORS ##
  if(varClass == "factor"){
    par(mar=c(0,0,0,0))
    out <-
      #plot(0,type='n',axes=FALSE,xlab='',ylab='',xlim=c(0,1),ylim=c(0,1))
      ## make a single horizontal "barplot" reflecting proportion of data removed
      barplot(matrix(c(length(selected)/total,
                       (1 - (length(selected)/total))), ncol=1),
              col=c("#ff9000", "grey"), space=0,
              axes=FALSE, xlab='', ylab='', xlim=c(-0.01, 1.01),
              horiz=TRUE, beside=FALSE)
    myText = paste(total-length(selected),'of',total,'levels removed')
    text(0,0.5, myText, pos=4, col="black", cex=1.3, font=2)
  }else{
    ## INTEGERS ##
    # restrict to small-ish number of levels
    if (length(tab)<50) {
      par(mar=c(0,0,0,0))
      z = as.numeric(names(tab))
      colVec = rep(grey(0.9),length(tab))
      colVec[selected] = '#ff9000'
      out <-
        barplot(tab/max(tab),col=colVec,space=0,axes=FALSE,
                xlab='',ylab='',ylim=c(-0.5,1.2))
      text(min(z)-0.5,0,min(z),pos=1)
      text(max(z)-0.5,0,max(z),pos=1)

      # otherwise produce simple summary box
    } else {
      par(mar=c(0,0,0,0))
      out <-
        #plot(0,type='n',axes=FALSE,xlab='',ylab='',xlim=c(0,1),ylim=c(0,1))
        ## make a single horizontal "barplot" reflecting proportion of data removed
        barplot(matrix(c(length(selected)/total,
                         (1 - (length(selected)/total))), ncol=1),
                col=c("#ff9000", "grey"), space=0,
                axes=FALSE, xlab='', ylab='', xlim=c(-0.01, 1.01),
                horiz=TRUE, beside=FALSE)
      myText = paste(total-length(selected),'of',
                     total,'unique values removed')
      text(0,0.5, myText, pos=4, col="black", cex=1.3, font=2)
    }
  }
  return(out)
} # end bobBarSubplot

###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###    ###
