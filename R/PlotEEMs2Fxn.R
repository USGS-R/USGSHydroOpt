#' plotEEMs2
#' 
#' Plot contour graph of excitation emmission spectra with defined peaks indicated
#' on the graph
#' 
#' @param mat 2-D matrix of excitation-emmission spectra
#' @param Ex numeric excitation wavelengths
#' @param Em numeric emmission wavelengths
#' @param nlevels numeric color levels for contour graph. 50 is commonly used for a value here.
#' @param Peaks dataframe with peaks to be indicated on the graph
#' @param peakCol character column name in Peaks which contains the abbreviation for that peak
#' @param peakEx character column name in Peaks to use for excitation wavelengths
#' @param peakEm character column name in Peaks to use for emmission wavelengths
#' @param mainTitle Plot title
#' @return Excitation-Emission (EEMs) Plot with the important peaks identified
#' @export
#' @examples
#' GRnum <- "gr13307"
#' mat <- a[,,GRnum]
#' Ex <- as.numeric(names(a[,1,1]))
#' Em <- as.numeric(names(a[1,,1]))
#' nlevels <- 50
#' Peaks <- ex_ems
#' peakCol <- "Peak"
#' peakEx <- "ExCA"
#' peakEm <- "EmCA"
#' titleSize <- 1.1
#' mainTitle <- "Example EEMs Plot"
#' exampleEEMs <- plotEEMs2(mat=mat,Ex=Ex,Em=Em,nlevels=nlevels,Peaks=Peaks,peakCol=peakCol,
#' peakEx=peakEx,peakEm=peakEm,mainTitle=mainTitle,titleSize=titleSize)
plotEEMs2 <- function(mat,Ex,Em,nlevels,Peaks,peakCol,peakEx,peakEm,mainTitle,titleSize){
  genericCensoringValue <- function(qualifier,value, detectionLimit){
    valueToUse <- ifelse("<" == qualifier, 0, value)    
    return(valueToUse)
  }
  
  rmNAcols <- function(df){
    #Function to remove all columns with all NAs  
    
    rmcols <- numeric()
    for (i in 1:ncol(df)){
      notNA <- sum(!is.na(df[,i]))
      if(notNA == 0) rmcols <- c(rmcols,i)
    }
    df <- df[,-rmcols]
    return(df)
  }
  
  
  
  colPal <- c(colors()[30], colors()[143],colors()[554])
  colfunc <- colorRampPalette(colPal)
  

  Ex <- as.numeric(Ex)
  Em <- as.numeric(Em)
  filled.contour(x=Ex,y=Em,z=mat,levels=c(1:50/50)*max(mat,na.rm=TRUE),
                 zlim = range(mat,na.rm=T),xlim = range(Ex), ylim = range(Em),
                 cex.axis=3,
                 #color.palette = rainbow(n=3),
                 col=colfunc(nlevels),#nlevels=nlevels,
                 plot.title = title (main = mainTitle,cex.main=titleSize,cex.lab=1.4,
                                     ylab = "emission wavelength (nm)",
                                     xlab = "excitation wavelength (nm)"),
                 key.title = title  (cex.main= 0.85,
                                     main="Intensity\n(RU)"),
                 
                 plot.axes = {lines(Peaks$ExCA["HI"==Peaks$Peak],Peaks$EmCA["HI"==Peaks$Peak],lwd=1,lty=2);
                              lines(Peaks$ExCA["FI"==Peaks$Peak],Peaks$EmCA["FI"==Peaks$Peak],lwd=1,lty=2);
                              text(Peaks$ExCA,Peaks$EmCA,labels=Peaks$Peak,font=2,cex=1);
                              axis(1);
                              axis(2);
                              
                 }
  )
}


