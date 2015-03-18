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
