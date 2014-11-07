#' getRatios
#'
#' Computes ratios from optical data. Assumes that the signal with the greatest 
#' mean is in the numerator making the mean ratio greater than one.
#'
#' @param dataSummary dataframe with summary absorbance and fluoresence data. 
#' @param sigs signals vector of variable names in dataSummary for generating ratios
#' @param grnum character column name that defines the column with sample names in the dataSummary dataframe.
#' These names are used to merge spectral slope data into the summary dataframe. 
#' @param specifyOrder boolean variable to signify whether the numerator and denominator 
#' shold stay as specified in ratioVars
#' @param recordOrder boolean variable to signify whether the numerator and denominator
#' order should be recorded and used as output rather than the actual ratios.
#' @param ratioVars dataframe with specific ratios specified. Numerators in the first column
#' and denominators in the second column
#' @return dataSummary dataframe with the additional columns of spectral ratios computed using getRatios
#' @export
#' @examples
#' dataSummary <- dfsummary
#' sigs <- ratioSignals[which(ratioSignals[2]>0),1]
#' grnum <- "GRnumber"
#' test <- getRatios(dataSummary,sigs,grnum)
getRatios <- function(dataSummary,sigs,grnum,specifyOrder=FALSE,recordOrder=FALSE,ratioVars=FALSE){
  
  ratios <- data.frame(dataSummary[,grnum])
  var1 <- character()
  var2 <- character()
  ratioOrders <- data.frame(var1,var2)
  names(ratios) <- grnum
  if(!specifyOrder){
    for(i in 1:(length(sigs)-1)){
      varName1 <- sigs[i]
      for(j in (i+1):length(sigs)){
        varName2 <- sigs[j]
        if(mean(dataSummary[,varName1],na.rm=TRUE) > (mean(dataSummary[,varName2],na.rm=TRUE))){
          ratio <- dataSummary[,varName1]/dataSummary[,varName2]
          ratioName <- paste("r",varName1,"_",varName2,sep="")
          if(recordOrder) ratioOrder <- c(varName1,varName2)
        }else{
          ratio <- dataSummary[,varName2]/dataSummary[,varName1]
          ratioName <- paste("r",varName2,"_",varName1,sep="")
          if(recordOrder) ratioOrder <- c(varName2,varName1)        
        }
        ratios <- cbind(ratios,ratio)
        names(ratios)[dim(ratios)[2]] <- ratioName
        if(recordOrder) ratioOrders <- rbind(ratioOrders,ratioOrder)        
      }
    }
  }else{
    for(i in 1:dim(ratioVars)[1]){
      varName1 <- ratioVars[1,i]
      varName2 <- ratioVars[2,i]
      ratio <- dataSummary[,varName1]/dataSummary[,varName2]
      ratioName <- paste("r",varName1,"_",varName2,sep="")
    }
  }
  
  if(recordOrder & !specifyOrder){
    return(ratioOrders)
  }else{
    dataSummary <- merge(dataSummary,ratios,by=grnum)
    return(dataSummary)
  }
  
}



