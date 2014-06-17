#' getIndexes
#'
#' Computes humification index and fluorescence indes from fluorescence data. HIX
#' as defined by Ohno, 2002,   Fluorescence inner-filtering correction for determining
#' the humification index of dissolved organic matter. Environ. Sci. Technol. 36: 742-746
#' doi: 10.1021/es0155276, 
#' HIX = sum(I435:I480)/(sum(I300:I345)+sum(I435:I480)) for ex=254
#' 
#' and FI_2005 as defined by Cory and McKnight, 2005, Fluorescence 
#' spectroscopy reveals ubiquitous presence of oxidized and reduced quinones in DOM. 
#' Environ. Sci. Technol. 39: 8142-8149, doi:10.1021/es0506962 and FI_2001 defined by
#' MCKNIGHT, D. M., E. W. BOYER, P. K. WESTERHOFF, P. T. DORAN, T. KULBE, AND 
#' D. T. ANDERSEN. 2001. Spectrofluorometric characterization of DOM for indication 
#' of precursor material and aromaticity. Limnol. Oceanogr. 46: 38-48, 
#' doi:10.4319/lo.2001.46.1.0038.
#' FI = ex370em470/ex370em520
#' 
#' and freshness index as defined by PARLANTI, E., K. WORZ, L. GEOFFROY, AND M. LAMOTTE. 
#' 2000. Dissolved organic matter fluorescence spectroscopy as a tool to estimate 
#' biological activity in a coastal zone submitted to anthropogenic inputs. Org. 
#' Geochem. 31: 1765-1781, doi:10.1016/S0146-6380(00)00124-8
#' FreshI = ex310em380/max(ex310 between em470 and em520)
#'
#' @param an array with 3-D fluorescence  results. The 3 dimensions are the excitation wavelength (character), the emission wavelength (character), and the sample number (character).
#' @param dataSummary dataframe with summary absorbance and fluoresence data. This 
#' function adds columns to the end of this dataframe as additional summary data.
#' @param grnum character column name that defines the grnumbers (sample numbers) in the dataSummary dataframe.
#' These names are used to merge spectral slope data into the summary dataframe. 
#' This function assumes the column names of the dataAbs are grnumbers as well.
#' @return dataSummary dataframe with the additional columns containing the humification and fluoresence indices.
#' @export
#' @examples
#' grnum <- "GRnumber"
#' dataSummary <- dfsummary
#' a <- a
#' test1 <- getIndexes(a,dataSummary)
getIndexes <- function(a,dataSummary,grnum){
  a <- a[,,dataSummary[,grnum]]
  
  grnums <- names(a[1,1,])
  Em <- as.numeric(names(a[1,,1]))
  Ex <- as.numeric(names(a[,1,1]))
  Ex254 <- which.min(abs(Ex-254))
  H1cols <- which(Em >=435 & Em<=480)
  H2cols <- which(Em >=300 & Em<=345)
  
  HIX <- numeric()
  FI <- numeric()
  FI_2001 <- numeric()
  FreshI <- numeric()
  for(j in 1:length(grnums)){
    if(sum(Ex==254)>0) {Ex254 <- a[Ex254,,j]
  }else{
    wv1 <- max(Ex[which(Ex<254)])
    wv2 <- min(Ex[which(Ex>254)])
    frac1 <- 1-(254-wv1)/(wv2-wv1)
    frac2 <- 1-(wv2-254)/(wv2-wv1)
    Ex254 <- frac1 * a[as.character(wv1),,j] + frac2 * a[as.character(wv2),,j]
    }
  HIX <- c(HIX,sum(Ex254[H1cols])/(sum(Ex254[H1cols]) + sum(Ex254[H2cols])))
  
  FI <- c(FI,a["370","470",j]/a["370","520",j])
  FI_2001 <- c(FI_2001,a["370","450",j]/a["370","500",j])
  which(Em==470)
  FreshI <- c(FreshI,a["310","380",j]/max(a["310",c(which(Em==470):which(Em==520)),j]))
  }
  Indices <- data.frame(GRnumber=grnums,HIX_2002=HIX,FI_2005=FI,FI_2001=FI_2001,FreshI=FreshI)
  merge(dataSummary,Indices,by=grnum,all=TRUE)
}
