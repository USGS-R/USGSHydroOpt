#' getRatios
#'
#' Computes ratios from optical data. Assumes that the signal with the greatest 
#' mean is in the numerator making the mean ratio greater than one.
#'
#' @param dataSummary dataframe with summary absorbance and fluoresence data. This 
#' @param signals vector of variable names in dataSummary for generating ratios
#' @param grnum Column name that defines the grnumbers in the dataSummary dataframe.
#' These names are used to merge ratio data into the summary dataframe. 
#' @export
#' 
# Example should eventually use these data after reducing the summary file to only a few columns:
# setwd("M:/QW Monitoring Team/GLRI toxics/Shared_optical_data/Summary variables")
# setwd("d:/srcldata/GLRI toxics/Shared_optical_data/Summary variables")
# dfSummary <- read.csv("testSummary.csv",as.is=TRUE)
# ratioSignals <- read.csv("ratioSignals.csv",as.is=TRUE)
# 
# dataSummary <- dfSummary
# signals <- ratioSignals[which(ratioSignals[2]>0),1]
# 
# test <- getRatios(dataSummary=dfSummary,signals=signals)
# grnum<-"GRnumber"
getRatios <- function(dataSummary,signals,grnum="GRnumber"){
  
  ratios <- data.frame(dataSummary[,grnum])
  names(ratios) <- grnum
  for(i in 1:(length(signals)-1)){
    varName1 <- signals[i]
    for(j in (i+1):length(signals)){
      varName2 <- signals[j]
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
    
    
    