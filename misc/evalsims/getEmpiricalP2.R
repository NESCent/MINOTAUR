returnEmpP <- function(AllObservedValues, NonCodingValues){

	getEmpP <- function(Obs, sort.Null){
      #Obs is a single observed value
      #sort.Null is a list of the null values in ascending order
      if(is.na(Obs)){
        return(NA)
      }else{
        options(warn=-1)
        out = max(which(sort.Null<=Obs))/length(sort.Null)
        if(is.infinite(out)==TRUE){out=0} ## above code is undefined when Obs < min(sort.Null)
        return(out)
      }
	} #end function

sapply(AllObservedValues, getEmpP, sort(NonCodingValues))
}


getEmpPower <- function(stat, neut.logic){
    #x <- qvalue(1-returnEmpP(stat, stat[neut.logic]))$q<0.01
    x <- qvalue(1-returnEmpP(stat, stat[neut.logic]))$q<0.05
    t.power <- table(x, neut.logic)#table(x, dat.out$s_high)
    t.power[2,1]/sum(t.power[,1])
}
