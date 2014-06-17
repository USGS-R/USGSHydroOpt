#' getRatios
#'
#' Computes ratios from optical data. Assumes that the signal with the greatest 
#' mean is in the numerator making the mean ratio greater than one.
#'
#' @param dataSummary dataframe with summary absorbance and fluoresence data. This 
#' @param signals vector of variable names in dataSummary for generating ratios
#' @param grnum Column name that defines the grnumbers in the dataSummary dataframe.
#' These names are used to merge ratio data into the summary dataframe. 
#' @return dataSummary dataframe with the additional columns of spectral ratios computed using getRatios
#' @export
#' @examples
#' dataSummary <- dfsummary
#' sigs <- ratioSignals[which(ratioSignals[2]>0),1]
#' grnum <- "GRnumber"
#' test <- getRatios(dataSummary,sigs,grnum)
getRatios <- function(dataSummary,sigs,grnum){
  
  ratios <- data.frame(dataSummary[,grnum])
  names(ratios) <- grnum
  for(i in 1:(length(sigs)-1)){
    varName1 <- sigs[i]
    for(j in (i+1):length(sigs)){
      varName2 <- sigs[j]
      if(mean(dataSummary[,varName1],na.rm=TRUE) > (mean(dataSummary[,varName2],na.rm=TRUE))){
        ratio <- dataSummary[,varName1]/dataSummary[,varName2]
        ratioName <- paste("r",varName1,"_",varName2,sep="")
      }else{
        ratio <- dataSummary[,varName2]/dataSummary[,varName1]
        ratioName <- paste("r",varName2,"_",varName1,sep="")
      }
      ratios <- cbind(ratios,ratio)
      names(ratios)[dim(ratios)[2]] <- ratioName
    }
  }
  
  dataSummary <- merge(dataSummary,ratios,by=grnum)
  return(dataSummary)
  
}
    
    
    