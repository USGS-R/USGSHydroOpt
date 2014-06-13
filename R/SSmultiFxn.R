#' getSag
#'
#' Computes spectral slopes from absorbance data using a linear regression to 
#' determine the first order decay function as defined in Helms et al. 2008, 
#' Limnol. Oceanogr., 53(3), 955-969. aL = aRef * exp(-S*(L-LRef)) where a = absorbance
#' coefficient, S = specral slope, and L = wavelength.
#'
#' @param dataAbs dataframe with absorbance spectra results, one column per sample
#' @param waveCol Column name to define the wavelengths for which absorbance was measured
#' @param signals dataframe with three columns. The first column represents the low wavelength,
#' the second column represents the high wavelength for which spectral slopes are
#' to be defined, and the third column is the variable name to be used. 
#' A spectral slope is computed for each row.
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
# signals <- dfSagSignals
# colSubsetString <- "gr"
# grnum <- "GRnumber"
# 
# 
# test <- getSag(dataAbs=dfabs,waveCol="wavelength",signals=dfSagSignals,
#               colSubsetString="gr",dataSummary=dataSummary,grnum="GRnumber")
# 

getSag <- function(dataAbs,waveCol="wavelength",signals,colSubsetString="gr",dataSummary,grnum="GRnumber"){
  df <- dataAbs[,grep(colSubsetString,names(dataAbs))]
  df <- df[,dataSummary[,grnum]]
  
  L <- dataAbs[,waveCol]
  
  for(j in 1:dim(signals)[1]){
    wvRows <- which(L>=signals[j,1] & L<=signals[j,2])
    Sag <- numeric()
    
    for(i in 1:dim(df)[2]){  
      aCorr <- df[wvRows,i]
      names(aCorr) <- dfabs[wvRows,waveCol]
      if(min(aCorr) <= 0)
      {minA <- min(aCorr[aCorr>0])
       aCorr[aCorr<=0] <- minA/2
      }
      
      y <- log(aCorr/aCorr[as.character(signals[j,2])])
      x <- L[wvRows]-L[which(L==signals[j,2])]
      Sag <- c(Sag,-coef(lm(y~x))[2])
    }
    #  names(Sag) <- names(df)
    SagName <- paste("Sag",signals[j,1],"_",signals[j,2],sep="")
    dfSag <- data.frame(Sag,names(df))
    names(dfSag) <- c(SagName,grnum)
    dataSummary <- merge(dataSummary,dfSag,by=grnum,all=TRUE)
  }
  return(dataSummary)
}


