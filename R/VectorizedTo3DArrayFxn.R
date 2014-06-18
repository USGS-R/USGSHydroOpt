#' VectorizedTo3DArray
#' 
#' Converts vectorized fluorescence dataframe into a 3-D array with Ex, Em, and
#' GRnumber as the dimensions. This results in one 2-D excitation-emmission array 
#' per sample. Requires the reshape2 package.
#' 
#' @import reshape2
#' @param df dataframe with vectorized fluorescence data in the format from the
#' CA WSC with one column containing both the Excitation and Emission pair, and
#' all other columns representing a sample (e.g.,GRnumber)
#' @param ExEm  the character name of column with Excitation and Emmission wavelengths
#' defined in this format: ###/###. For example, Excitation 250 and emmission 400
#' would be represented as "250/400". 
#' @return an array with 3-D fluorescence  results. The 3 dimensions are the excitation wavelength (character), the emission wavelength (character), and the sample number, e.g.,"GRnumber" (character).
#' @export
#' @examples
#' df <- dfFluor
#' ExEm <- "Wavelength.Pairs"
#' aTest <- VectorizedTo3DArray(df,ExEm)
VectorizedTo3DArray <- function(df,ExEm){
  dfV <- cbind(read.table(textConnection(df$Wavelength.Pairs),sep="/"),df[,2:dim(df)[2]])
  colnames(dfV)[1] <- "Ex"
  colnames(dfV)[2] <- "Em"
  m <- melt(data=dfV,id=c("Ex","Em"))
  colnames(m)[3] <- "GRnumber"
  a <- acast(m,Ex~Em~GRnumber)
  return(a)
}
