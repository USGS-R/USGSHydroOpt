#' getExpResid
#'
#' Computes residuals from a linear regression using the first order decay function as defined in Helms et al. 
#' 2008, Limnol. Oceanogr., 53(3), 955-969.  Function assumes that the column names of the absorbance data file 
#' being used are formatted as grnumbers.
#' @param wavelength absorbance numeric wavelength 
#' @param rangeReg numeric string with absorbance wavelength range to be considered for computing spectral slope
#' @param rangeGap numeric string with the absorbance wavelength range for which decay function should be applied
#' @param dataAbs dataframe with absorbance spectra results, one column per sample, and one column
#' containing the wavelength at which an absorbance measurment is made. 
#' @param waveCol character column name to define the wavelengths for which absorbance was measured
#' @param colSubsetString unique characters to identify which columns contain absorbance data. 
#' The default is "gr" to comply with the common naming from the CA WSC. The sample names  must begin with
#' 1-n characters for the function to work. 
#' @param dataSummary dataframe with summary absorbance and fluoresence data. This 
#' function adds columns to the end of this dataframe as additional summary data.
#' @param grnum character column name that defines the column with sample names in the dataSummary dataframe.
#' @param plotResid boolean variable indicating wether plots of the absorbance coefficients with the regression
#' result should be plotted.
#' @return dataframe with the added spectral slope for each sample and plots with the absorbance spectra for rangeReg showing the 
#' model constructed using the spectral slope (red); and the absorbance data where black = the data in rangeReg that is not in rangeGap and blue = the data from rangeGap.
#' @export 
#' @examples
#' wavelength <- 267
#' rangeReg <- c(240,340)
#' rangeGap <- c(255,300)
#' dataAbs <- dfabs
#' waveCol <- "wavelengths"
#' colSubsetString <- "gr"
#' dataSummary <- dfsummary
#' grnum <- "GRnumber"
#' testdfOpt <- getExpResid(wavelength,rangeReg,rangeGap,dataAbs,waveCol,
#'                     colSubsetString,dataSummary,grnum)
getExpResid <- function(wavelength,rangeReg,rangeGap,dataAbs,waveCol,colSubsetString,dataSummary,grnum,plotResid=FALSE){
  df <- dataAbs[,grep(colSubsetString,names(dataAbs))]
  grnums <- as.character(dataSummary[,grnum])
  df <- df[,grnums]
  L <- dataAbs[,waveCol]
  rangeReg[2] <- L[which(abs(L-rangeReg[2])==min(abs(L-rangeReg[2])))]
  aRef <- which(dataAbs[,waveCol]==rangeReg[2])
  
  wvRows <- which((L>=rangeReg[1] & L<=rangeGap[1]) | (L>=rangeGap[2] & L<=rangeReg[2]))
  wvRowsAll <- which((L>=rangeReg[1] & L<=rangeReg[2]))
  wvRowsGap <- wvRowsAll[which(!wvRowsAll %in% wvRows)]
  AResids <- numeric()
  aWavelngth <- which.min(abs(dataAbs[wvRowsAll,waveCol]-wavelength))
  
  for(i in 1:dim(df)[2]){  
    aCoef <- df[wvRows,i]
    names(aCoef) <- dataAbs[wvRows,waveCol]
    if(sum(aCoef)>0){
      if(min(aCoef) <= 0)
      {minA <- min(aCoef[aCoef>0])
       aCoef[aCoef<=0] <- minA/2
      }
      y <- log(aCoef/aCoef[as.character(rangeReg[2])])
      x <- L[wvRows]-L[which(L==rangeReg[2])]
      m <- lm(y~x)
      Sag <- -coef(m)[2]
      aLAll <- df[aRef,i] * exp(-Sag*(dataAbs[wvRowsAll,waveCol]-rangeReg[2]))
      AResidsAll <- df[wvRowsAll,i] - aLAll
      AResid <- AResidsAll[aWavelngth]     
      residRow <- which(dataAbs[wvRowsAll,waveCol]==wavelength)
      
      if(plotResid){
        plot(df[wvRowsAll,i]~dataAbs[wvRowsAll,waveCol],main=names(df)[i])
        points(df[wvRowsGap,i]~dataAbs[wvRowsGap,waveCol],col="blue")
        lines(aLAll~dataAbs[wvRowsAll,waveCol],col="red")
      }
    }else{AResid <- NA
    }
    AResids <- c(AResids,AResid)
  }
  dfResids <- data.frame(grnums,AResids)
  names(dfResids) <- c(grnum,paste("Aresid",wavelength,sep=''))
  dataSummaryFinal <- merge(dataSummary,dfResids,by=grnum,all=TRUE)
  return(dataSummaryFinal)
}
