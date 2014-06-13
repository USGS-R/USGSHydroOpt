#' getSag
#'
#' Computes spectral slopes from absorbance data using a linear regression to 
#' determine the first order decay function as defined in Helms et al. 2008, 
#' Limnol. Oceanogr., 53(3), 955-969. aL = aRef * exp(-S*(L-LRef)) where a = absorbance
#' coefficient, S = spectral slope, and L = wavelength.
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
#' @export
#Example should eventually use these data after reducing the summary file to only a few columns:
# setwd("M:/QW Monitoring Team/GLRI toxics/Shared_optical_data/excel_files")
# # setwd("d:/srcldata/GLRI toxics/Shared_optical_data/excel_files")
# dfabs <- read.csv("../Final data/compiled_absorbance_Corr_May2014.csv",as.is=TRUE)
# dfsummary <- read.csv("GLRIOpticalSummary011914.csv",as.is=TRUE)
# dfSummary <- read.csv("../Final data/Original data/GLRI_Summary_091913.csv",as.is=TRUE)
# dfSagSignals <- read.csv("../Summary variables/SagWaves.csv",as.is=TRUE)
# dfIDs <- read.csv("../Final data/dfLabIDs.csv",as.is=TRUE)
# dfIDs <- dfIDs[!is.na(dfIDs$GRnumber),]
# 
# 
# dataAbs <- dfabs
# waveCol <- "wavelength"
# dfSum <- data.frame(grnumber=grep("gr",names(dfabs),value=TRUE))
# dataSummary <- dfIDs
# signals <- dfSagSignals
# colSubsetString <- "gr"
# grnum <- "GRnumber"
# rangeReg <- c(240,340)
# rangeGap <- c(255,300)
# wavelngth <- 267
# pdf("../Graphics/expResid.pdf")
# test <- getExpResid(wavelngth,rangeReg,rangeGap,dataAbs=dfabs,waveCol="wavelength",
#                     colSubsetString="gr",dataSummary=dfIDs,grnum="GRnumber")
# dfOpt2 <- getExpResid(wavelngth=267,rangeReg=c(240,340),rangeGap=c(255,300),
#                       dataAbs=dfabs,waveCol="wavelength",colSubsetString="gr",
#                       dataSummary=dfOpt2,grnum="GRnumber")
# 
# dev.off()
getExpResid <- function(wavelngth,rangeReg,rangeGap,dataAbs,waveCol="wavelength",colSubsetString="gr",dataSummary,grnum="GRnumber"){
  df <- dataAbs[,grep(colSubsetString,names(dataAbs))]
  grnums <- as.character(dataSummary[,grnum])
  df <- df[,grnums]
  aRef <- which(dataAbs[,waveCol]==rangeReg[2])
  aStart <- which(dataAbs[,waveCol]==rangeReg[2])
  L <- dataAbs[,waveCol]
    
  wvRows <- which((L>=rangeReg[1] & L<=rangeGap[1]) | (L>=rangeGap[2] & L<=rangeReg[2]))
  wvRowsAll <- which((L>=rangeReg[1] & L<=rangeReg[2]))
  wvRowsGap <- wvRowsAll[which(!wvRowsAll %in% wvRows)]
  AResids <- numeric()
  aWavelngth <- which(dataAbs[wvRowsAll,waveCol]==wavelngth)  
  
  for(i in 1:dim(df)[2]){  
    aCoef <- df[wvRows,i]
    names(aCoef) <- dfabs[wvRows,waveCol]
    if(sum(aCoef>0)){
      if(min(aCoef) <= 0)
      {minA <- min(aCoef[aCoef>0])
       aCoef[aCoef<=0] <- minA/2
      }
      y <- log(aCoef/aCoef[as.character(rangeReg[2])])
      x <- L[wvRows]-L[which(L==rangeReg[2])]
      m <- lm(y~x)
      Sag <- -coef(m)[2]

 ########     determine all residuals, then pick out the chosen residual
      
      aLAll <- df[aRef,i] * exp(-Sag*(dataAbs[wvRowsAll,waveCol]-rangeReg[2]))
    AResidsAll <- df[wvRowsAll,i] - aLAll
   AResid <- AResidsAll[aWavelngth]     
    AResids <- c(AResids,AResid)
    residRow <- which(dataAbs[wvRowsAll,waveCol]==wavelngth)
#     AResid <- aLAll[residRow] df[wvRowsAll,i]-aLAll
    plot(df[wvRowsAll,i]~dataAbs[wvRowsAll,waveCol],main=names(df)[i])
    points(df[wvRowsGap,i]~dataAbs[wvRowsGap,waveCol],col="blue")
    
    lines(aLAll~dataAbs[wvRowsAll,waveCol],col="red")
    }else{AResid <- NA
    }
    
    
    # a = absorbance
    # coefficient, S = specral slope, and L = wavelength.
    # aL = aRef * exp(-S*(L-LRef)) 
    
  }
  dfResids <- data.frame(grnums,AResids)
  names(dfResids) <- c(grnum,"Aresids")
  dataSummary <- merge(dataSummary,dfResids,by=grnum,all=TRUE)
  
  return(dataSummary)
}

