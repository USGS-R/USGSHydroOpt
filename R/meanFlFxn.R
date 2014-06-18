#' getMeanFl
#'
#' 
#' and freshness index as defined by PARLANTI, E., K. WORZ, L. GEOFFROY, AND M. LAMOTTE. 
#' 2000. Dissolved organic matter fluorescence spectroscopy as a tool to estimate 
#' biological activity in a coastal zone submitted to anthropogenic inputs. Org. 
#' Geochem. 31: 1765-1781, doi:10.1016/S0146-6380(00)00124-8
#' FreshI = ex310em380/max(ex310 between em470 and em520)
#'
#' @param a an array with 3-D fluorescence  results. The 3 dimensions are the excitation wavelength (character), the emission wavelength (character), and the sample number (character). 
#' This function assumes names of the third dimension in this array are sample numbers (GRnumber)
#' @param signals dataframe defining the max and min excitation (integer) and the max and 
#' min emmission (integer) wavelengths for which to compute averages. Contains one column (character) with the names of the various parameters (e.g.,OB1,S1.50,B,T).
#' @param Peak character column for the column in signals with parameters to be computed
#' @param Ex1 the first integer excitation wavelength in the range
#' @param Ex2 the second integer excitation wavelength in the range. This can be blank 
#' if one specific excitation wavelength is used.
#' @param Em1 the first integer emmission wavelength in the range
#' @param Em2 the second integer emmission wavelength in the range. This can be blank 
#' if one specific emmission wavelength is used.
#' @param dataSummary dataframe with summary absorbance and fluoresence data. This 
#' function adds columns to the end of this dataframe as additional summary data.
#' @param grnum character column name that defines the grnumbers in the dataSummary dataframe. 
#' @return dataSummary dataframe with the additional freshness index columns.
#' @export
#' @examples
#' a <- a
#' signals <- signals
#' Peak <- "Peak"
#' Ex1 <- "Ex1"
#' Ex2 <- "Ex2"
#' Em1 <- "Em1"
#' Em2 <- "Em2"
#' dataSummary <- dfsummary
#' grnum <- "GRnumber"
#' testMeanFl <- getMeanFl(a,signals,Peak,Ex1,Ex2,Em1,Em2,dataSummary,grnum)
getMeanFl <- function(a,
                      signals,Peak,Ex1,Ex2,Em1,Em2,
                      dataSummary,grnum){
  a <- a[,,dataSummary[,grnum]]
  grnums <- names(a[1,1,])
  dfMeanFl <- data.frame(GRnumber=grnums)
  names(dfMeanFl) <- grnum
  Em <- as.numeric(names(a[1,,1]))
  Ex <- as.numeric(names(a[,1,1]))
  signals[,Ex2] <- ifelse(is.na(signals[,Ex2]),signals[,Ex1],signals[,Ex2])
  signals[,Em2] <- ifelse(is.na(signals[,Em2]),signals[,Em1],signals[,Em2])
  
  for (i in 1:dim(signals)[1]){
    
    sEx1 <- which.min(abs(Ex-signals[i,Ex1]))
    sEx2 <- which.min(abs(Ex-signals[i,Ex2]))
    sEm1 <- which.min(abs(Em-signals[i,Em1]))
    sEm2 <- which.min(abs(Em-signals[i,Em2]))
    
    MeanFl <- numeric()
    if(sEx1!=sEx2 & sEm1!=sEm2){
      for (j in 1:length(grnums)){
        a2 <- a[c(sEx1:sEx2),c(sEm1:sEm2),grnums]
        MeanFl <- c(MeanFl,mean(a2[,,j]))
      }
    }else{if(sEx1==sEx2 & sEm1!=sEm2){
      for (j in 1:length(grnums)){
        a2 <- a[sEx1,c(sEm1:sEm2),grnums]
        MeanFl <- c(MeanFl,mean(a2[,j]))
      }
    }else{if(sEx1!=sEx2 & sEm1==sEm2){
      for (j in 1:length(grnums)){
        a2 <- a[c(sEx1,sEx2),sEm1,grnums]
        MeanFl <- c(MeanFl,mean(a2[,j]))
      }
    }else{if(sEx1==sEx2 & sEm1==sEm2){
      pair <- a[sEx1,sEm1,grnums]
      MeanFl <- c(MeanFl,pair)
    }
    }
    }
    }
    dfMeanFl <- cbind(dfMeanFl,MeanFl)
  }
  names(dfMeanFl) <- c(grnum,signals[,Peak])
  dfReturn <- merge(dataSummary,dfMeanFl,all=TRUE)
  return(dfReturn)
}
