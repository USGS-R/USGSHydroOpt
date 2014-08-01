#' getSag
#'
#' Computes spectral slopes from absorbance data using a linear regression to 
#' determine the first order decay function as defined in Helms et al. 2008, 
#' Limnol. Oceanogr., 53(3), 955-969. aL = aRef * exp(-S*(L-LRef)) where a = absorbance
#' coefficient, S = specral slope, and L = wavelength.
#'
#' @param dataAbs dataframe with absorbance spectra results, one column per sample, and one column
#' containing the wavelength at which an absorbance measurment is made. 
#' @param waveCol  column name as character to define the wavelengths (as integer) for which absorbance was measured
#' @param sag dataframe with three columns. The first column represents the low wavelength (as integer),
#' the second column represents the high wavelength (as integer) for which spectral slopes are
#' to be defined, and the third column is the variable name to be used (as factor). 
#' A spectral slope is computed for each row.
#' @param colSubsetString unique characters to identify which columns contain absorbance data. 
#' The default is "gr" to comply with the common naming from the CA WSC. The sample names  must begin with
#' 1-n characters for the function to work. 
#' @param dataSummary dataframe with summary absorbance and fluorescence data. This 
#' function adds columns to the end of this dataframe as additional summary data.
#' @param grnum character column name that defines the column with sample names in the dataSummary dataframe.
#' These names are used to merge spectral slope data into the summary dataframe. 
#' @return dataSummary dataframe with the additional columns containing spectral slopes as defined in sag for each sample (e.g., GRnumber).
#' @export
#' @examples
#' dataAbs <- dfabs
#' waveCol <- "wavelengths"
#' sag <- dfsags
#' colSubsetString <- "gr"
#' dataSummary <- dfsummary
#' dataSummary <- dataSummary[,-c(64:67)] #remove columns with spectral slopes and re-compute with this function
#' grnum <- "GRnumber"
#' testSag <- getSag(dataAbs,waveCol,sag,colSubsetString,dataSummary,grnum)
getSag <- function(dataAbs,waveCol,sag,colSubsetString,dataSummary,grnum){
  df <- dataAbs[,grep(colSubsetString,names(dataAbs))]
  df <- df[,dataSummary[,grnum]]
  
  L <- dataAbs[,waveCol]
  
  for(j in 1:dim(sag)[1]){
    
    sag[j,1] <- L[which(abs(L-sag[j,1])==min(abs(L-sag[j,1])))]
    sag[j,2] <- L[which(abs(L-sag[j,2])==min(abs(L-sag[j,2])))]
    
    wvRows <- which(L>=sag[j,1] & L<=sag[j,2])
    Sag <- numeric(length(grep(colSubsetString,names(dataAbs))))
    
    for(i in 1:dim(df)[2]){  
      aCorr <- df[wvRows,i]
      names(aCorr) <- dataAbs[wvRows,waveCol]
      
      if(all(aCorr<0)){
        minA <- min(abs(aCorr[aCorr<0]))
        aCorr[aCorr<=0] <- minA/2
      }else{
        if(min(aCorr) <= 0)
        {minA <- min(aCorr[aCorr>0])
         aCorr[aCorr<=0] <- minA/2
        }
      }
      y <- log(aCorr/aCorr[as.character(sag[j,2])])
      x <- L[wvRows]-L[which(L==sag[j,2])]
      Sag[i] <- -coef(lm(y~x))[2]
    }
    SagName <- paste("Sag",sag[j,1],"_",sag[j,2],sep="")
    dfSag <- data.frame(Sag,names(df),row.names=NULL)
    names(dfSag) <- c(SagName,grnum)
    dataSummary <- merge(dataSummary,dfSag,by=grnum,all=TRUE)
  }
}
return(dataSummary)
}


