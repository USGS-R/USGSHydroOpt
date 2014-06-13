#' getLog10
#'
#' Computes log transform of optical summary data. 
#'
#' @param dataSummary dataframe with summary absorbance and fluoresence data.
#' @param signals vector of variable names in dataSummary for generating log transforms
#' @param grnum Column name that defines the grnumbers in the dataSummary dataframe.
#' These names are used to merge ratio data into the summary dataframe. 
#' @export
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
getLog10 <- function(dataSummary,signals,grnum="GRnumber"){
  
  ratios <- data.frame(dataSummary[,grnum])
  names(ratios) <- grnum
  for(i in 1:(length(signals))){
    dataSummary <- cbind(dataSummary,log10(dataSummary[,signals[i]]))
    names(dataSummary)[dim(dataSummary)[2]] <- paste("log",signals[i],sep="")
  
  }
  
  return(dataSummary)
  
}
    
    
    