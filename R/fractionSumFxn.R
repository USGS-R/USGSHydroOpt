#' getFrcSum
#'
#' Computes the fraction of the sum of two signals from optical data as:
#' Fraction = variable 1 / (variable 1 + variable 2)
#'
#' @param dataSummary dataframe with summary absorbance and fluoresence data. 
#' @param sigs signals vector of variable names in dataSummary for generating fractions
#' @param grnum character column name that defines the column with sample names in the dataSummary dataframe.
#' These names are used to merge fraction data into the summary dataframe. 
#' @return dataSummary dataframe with the additional columns of spectral fractions computed using getFrcSum
#' @export
#' @examples
#' dataSummary <- dfsummary
#' sigs <- fractionSignals
#' grnum <- "GRnumber"
#' test <- getFrcSum(dataSummary,sigs,grnum)
getFrcSum <- function(dataSummary,sigs,grnum,specifyOrder=FALSE,recordOrder=FALSE,fractionVars=FALSE){
  
  fractions <- data.frame(dataSummary[,grnum])
  var1 <- character()
  var2 <- character()
  names(fractions) <- grnum
  for(i in 1:(dim(sigs)[1])){
    varName1 <- sigs[i,1]
    varName2 <- sigs[i,2]
    fraction <- dataSummary[,varName1]/(dataSummary[,varName1] + dataSummary[,varName2])
    fractionName <- paste("fs",varName1,"_",varName2,sep="")
    fractions <- cbind(fractions,fraction)
    names(fractions)[dim(fractions)[2]] <- fractionName
  }
  dataSummary <- merge(dataSummary,fractions,by=grnum)
  return(dataSummary)
}



