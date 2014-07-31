#' getLog10
#'
#' Computes log transform of optical summary data. 
#'
#' @param dataSummary dataframe with summary absorbance and fluoresence data.
#' @param signals character vector of variable names in dataSummary for generating log transforms
#' @param grnum character column name that defines the column with sample names in the dataSummary dataframe.
#' These names are used to merge ratio data into the summary dataframe. 
#' @return dataframe with the log 10 transform of the summary absorbance and fluoresence data.
#' @export
#' @examples
#' dataSummary <- dfsummary
#' signals <- ratioSignals[which(ratioSignals[2]>0),1]
#' grnum<-"GRnumber"
#' test2 <- getLog10(dataSummary,signals,grnum)
getLog10 <- function(dataSummary,signals,grnum){
  
  ratios <- data.frame(dataSummary[,grnum])
  names(ratios) <- grnum
  for(i in 1:(length(signals))){
    dataSummary <- cbind(dataSummary,log10(dataSummary[,signals[i]]))
    names(dataSummary)[dim(dataSummary)[2]] <- paste("log",signals[i],sep="")
  
  }
  
  return(dataSummary)
  
}
    
    
    