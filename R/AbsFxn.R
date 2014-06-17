#' getAbs
#'
#' Retrieves individual absorbance coefficients
#'
#' @param dataAbs dataframe with absorbance spectra results, one column per sample, and one column with the wavelengths.
#' @param waveCol character column name to define the wavelengths for which absorbance was measured.
#' @param signals vector with absorbance wavelengths to extract.
#' @param colSubsetString unique characters to identify which columns have absorbance data. 
#' The default is "gr" to comply with the common naming from the CA WSC.
#' @param dataSummary dataframe with summary absorbance and fluoresence data. This 
#' function adds columns to the end of this dataframe as additional summary data.
#' @param grnum Column name that defines the grnumbers in the dataSummary dataframe.
#' These names are used to merge spectral slope data into the summary dataframe. 
#' This function assumes the column names of the dataAbs are grnumbers as well.
#' @return summary absorbance and fluoresence dataframe with the additional absorbance peaks extracted using getAbs
#' @examples
#' dataAbs <- dfabs
#' waveCol <- "wavelengths"
#' wavs <- c(430,530,630,730)
#' colSubsetString <- "gr"
#' dataSummary <- dfsummary
#' grnum <- "GRnumber"
#' testAbs <- getAbs(dataAbs,waveCol,wavs,
#'                colSubsetString,dataSummary,grnum)
getAbs <- function(dataAbs,waveCol,signals,colSubsetString,dataSummary,grnum){
  df <- dataAbs[,grep(colSubsetString,names(dataAbs))]
  df <- df[,dataSummary[,grnum]]
  grnums <- dataSummary[,grnum]
  L <- dataAbs[,waveCol]
  
  dfAbsSig <- data.frame(GRnumber=grnums)
  
  for(j in 1:length(wavs)){
    AbsCol <- which(dataAbs[,waveCol]==wavs[j])
    A <- as.numeric(df[AbsCol,])
    dfAbsSig <- cbind(dfAbsSig,A)
  }
  
  Anames <- paste("A",signals,sep="")
  names(dfAbsSig) <- c(grnum,Anames)
  dataSummary <- merge(dataSummary,dfAbsSig,by=grnum)
  return(dataSummary)
}


