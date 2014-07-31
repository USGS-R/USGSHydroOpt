#' getIndexes
#'
#' Computes humification index and fluorescence indes from fluorescence data. HIX
#' as defined by Ohno, 2002,   Fluorescence inner-filtering correction for determining
#' the humification index of dissolved organic matter. Environ. Sci. Technol. 36: 742-746
#' doi: 10.1021/es0155276, HIX = sum(I435:I480)/(sum(I300:I345)+sum(I435:I480)) for ex=254 and FI_2005 as defined by Cory and McKnight, 2005, 
#' Fluorescence spectroscopy reveals ubiquitous presence of oxidized and reduced quinones in DOM. Environ. Sci. Technol. 39: 8142-8149, 
#' doi:10.1021/es0506962 and FI_2001 defined by MCKNIGHT, D. M., E. W. BOYER, P. K. WESTERHOFF, P. T. DORAN, T. KULBE, AND D. T. ANDERSEN. 2001. 
#' Spectrofluorometric characterization of DOM for indication of precursor material and aromaticity. Limnol. Oceanogr. 46: 38-48, doi:10.4319/lo.2001.46.1.0038.
#' FI = ex370em470/ex370em520 and freshness index as defined by PARLANTI, E., K. WORZ, L. GEOFFROY, AND M. LAMOTTE. 
#' 2000. Dissolved organic matter fluorescence spectroscopy as a tool to estimate biological activity in a coastal zone submitted to anthropogenic inputs. Org. Geochem. 31: 1765-1781, doi:10.1016/S0146-6380(00)00124-8
#' FreshI = ex310em380/max(ex310 between em470 and em520),
#'
#' @param a an array with 3-D fluorescence  results. The 3 dimensions are the excitation wavelength (character), the emission wavelength (character), and the sample names (character).
#' @param dataSummary dataframe with summary absorbance and fluoresence data. This 
#' function adds columns to the end of this dataframe as additional summary data.
#' @param grnum character column name that defines the column with sample names in the dataSummary dataframe.
#' These names are used to merge spectral slope data into the summary dataframe. 
#' @return dataSummary dataframe with the additional columns containing the humification and fluoresence indices.
#' @export
#' @examples
#' a <- a
#' dataSummary <- dfsummary 
#' dataSummary <- dataSummary[,-c(43:46)] #remove columns with fluoresence and humic index
#' grnum <- "GRnumber"
#' test1 <- getIndexes(a,dataSummary,grnum)
getIndexes <- function(a,dataSummary,grnum){
  a <- a[,,dataSummary[,grnum]]
  
  grnums <- names(a[1,1,])
  Em <- as.numeric(names(a[1,,1]))
  Ex <- as.numeric(names(a[,1,1]))
  Ex254 <- which.min(abs(Ex-254))
  H1cols <- which(Em >=435 & Em<=480)
  H2cols <- which(Em >=300 & Em<=345)
  
  HIX <- numeric(length(grnums))
  FI <- numeric(length(grnums))
  FI_2001 <- numeric(length(grnums))
  FreshI <- numeric(length(grnums))
  
  FIEx <- as.character(Ex[which(abs(Ex-370)==min(abs(Ex-370)))])
  FIEx <- ifelse(length(FIEx>1),FIEx<-FIEx[1],FIEx)
  
  FIEmLower <- as.character(Em[which(abs(Em-470)==min(abs(Em-470)))])
  FIEmLower <- ifelse(length(FIEmLower>1),FIEmLower<-FIEmLower[1],FIEmLower)
  
  FIEmUpper <- as.character(Em[which(abs(Em-520)==min(abs(Em-520)))])
  FIEmUpper <- ifelse(length(FIEmUpper>1),FIEmUpper<-FIEmUpper[1],FIEmUpper)
  
  FIEmLower1 <- as.character(Em[which(abs(Em-450)==min(abs(Em-450)))])
  FIEmLower1 <- ifelse(length(FIEmLower1>1),FIEmLower1<-FIEmLower1[1],FIEmLower1)
  
  FIEmUpper1 <- as.character(Em[which(abs(Em-500)==min(abs(Em-500)))])
  FIEmUpper1 <- ifelse(length(FIEmUpper1>1),FIEmUpper1<-FIEmUpper1[1],FIEmUpper1)
  
  FreshIEx <- as.character(Ex[which(abs(Ex-310)==min(abs(Ex-310)))])
  FreshIEx <- ifelse(length(FreshIEx>1),FreshIEx<-FreshIEx[1],FreshIEx)
  
  FreshIEm <- as.character(Em[which(abs(Em-380)==min(abs(Em-380)))])
  FreshIEm <- ifelse(length(FreshIEm>1),FreshIEm<-FreshIEm[1],FreshIEm)
  
  EmRange <- FIEmLower:FIEmUpper
  
  for (i in 1:length(EmRange)){
    tempEm <- Em[which(abs(Em-EmRange[i])==min(abs(Em-EmRange[i])))]
    EmRange[i] <- ifelse(length(tempEm>1),EmRange[i] <- tempEm[1],tempEm)
  }
  
  Dups <- which(duplicated(EmRange))
  EmRange <- as.character(EmRange[-c(Dups)])
  
  for(j in 1:length(grnums)){
    if(sum(Ex==254)>0) {Ex254 <- a[Ex254,,grnums[j]]
    }else{
      wv1 <- max(Ex[which(Ex<254)])
      wv2 <- min(Ex[which(Ex>254)])
      frac1 <- 1-(254-wv1)/(wv2-wv1)
      frac2 <- 1-(wv2-254)/(wv2-wv1)
      Ex254 <- frac1 * a[as.character(wv1),,grnums[j]] + frac2 * a[as.character(wv2),,grnums[j]]
    }
    
    HIX[j] <- ifelse((sum(Ex254[H1cols]) + sum(Ex254[H2cols]))==0,HIX[j] <- 0, HIX[j] <- (sum(Ex254[H1cols]))/((sum(Ex254[H1cols]) + sum(Ex254[H2cols]))))
    
    FI[j] <- ifelse(a[FIEx,FIEmUpper,grnums[j]]==0, FI[j] <-0, FI[j] <- (a[FIEx,FIEmLower,grnums[j]])/(a[FIEx,FIEmUpper,grnums[j]]))
    
    FI_2001[j] <- ifelse(a[FIEx,FIEmUpper,grnums[j]]==0, FI_2001[j] <- 0, FI_2001[j] <- (a[FIEx,FIEmLower1,grnums[j]])/(a[FIEx,FIEmUpper,grnums[j]]))
    
    FreshI[j] <- ifelse(max(a[FreshIEx,c(EmRange),grnums[j]])==0,FreshI[j] <- 0, FreshI[j] <- (a[FreshIEx,FreshIEm,grnums[j]])/(max(a[FreshIEx,c(EmRange),grnums[j]])))
  }
  
  Indices <- data.frame(sampleNames=grnums,HIX_2002=HIX,FI_2005=FI,FI_2001=FI_2001,FreshI=FreshI)
  merge(dataSummary,Indices,by.x=grnum,by.y='sampleNames',all=TRUE)
}
