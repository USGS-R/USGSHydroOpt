#' getAbs
#'
#' Retrieves individual absorbance coefficients
#'
#' @param dataAbs dataframe with absorbance spectra results, one column per sample
#' @param waveCol Column name to define the wavelengths for which absorbance was measured
#' @param signals vector with absorbance wavelengths to extract.
#' @param colSubsetString unique characters to identify which columns have absorbance data. 
#' The default is "gr" to comply with the common naming from the CA WSC
#' @param dataSummary dataframe with summary absorbance and fluoresence data. This 
#' function adds columns to the end of this dataframe as additional summary data.
#' @param grnum Column name that defines the grnumbers in the dataSummary dataframe.
#' These names are used to merge spectral slope data into the summary dataframe. 
#' This function assumes the column names of the dataAbs are grnumbers as well.
#' 


# Example should eventually use these data after reducing the summary file to only a few columns:
# setwd("M:/QW Monitoring Team/GLRI toxics/Shared_optical_data/excel_files")
# dfabs <- read.csv("Compiled_absorbance_corrected_24March2014.csv")
# dfsummary <- read.csv("GLRIOpticalSummary011914.csv")
# 
# 
# dataAbs <- dfabs
# waveCol <- "wavelength"
# dfSum <- data.frame(grnumber=grep("gr",names(dfabs),value=TRUE))
# dataSummary <- dfOpt2
# signals <- dfAbsSignals[,1]
# colSubsetString <- "gr"
# grnum <- "GRnumber"
# 
# 
# test <- getAbs(dataAbs=dfabs,waveCol="wavelength",signals=signals,
#                colSubsetString="gr",dataSummary=dataSummary,grnum="GRnumber")


getAbs <- function(dataAbs,waveCol="wavelength",signals,colSubsetString="gr",dataSummary,grnum="GRnumber"){
  df <- dataAbs[,grep(colSubsetString,names(dataAbs))]
  df <- df[,dataSummary[,grnum]]
  grnums <- dataSummary[,grnum]
  L <- dataAbs[,waveCol]
  
  dfAbsSig <- data.frame(GRnumber=grnums)
  
  for(j in 1:length(signals)){
    AbsCol <- which(dataAbs[,waveCol]==signals[j])
    A <- as.numeric(df[AbsCol,])
    dfAbsSig <- cbind(dfAbsSig,A)
  }
  
  Anames <- paste("A",signals,sep="")
  names(dfAbsSig) <- c(grnum,Anames)
  dataSummary <- merge(dataSummary,dfAbsSig,by=grnum)
  return(dataSummary)
}


