#' plotAbs
#' 
#' Plot absorbance coefficient curves
#' 
#' @param dataAbs dataframe of absorbance data
#' @param WaveCol character column name to define the wavelengths for which absorbance was measured.
#' @param absCol character column name to define which column to plot
#' @param ... additional plotting parameters as needed. xlim and ylim are commonly used here.
#' @return Absorbance coefficient curve
#' @export
#' @examples
#' WaveCol <- "wavelengths"
#' absCol <- "gr13307"
#' titleSize <- 1.1
#' mainTitle <- "Example Absorbance Plot"
#' xlim <- c(239,450)
#' dataAbs <- dfabs
#' plotAbs(dataAbs,WaveCol,absCol,main=mainTitle,cex.main=titleSize,xlim=xlim)

plotAbs <- function(dataAbs,WaveCol,absCol,...){

  plot(dataAbs[,WaveCol],dataAbs[,absCol],
       type="l",lty=1,col="blue",
       xlab="Wavelength (nm)",
       ylab="Absorbance coefficient",
       ...)
  
}

