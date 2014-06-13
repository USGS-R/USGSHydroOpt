#' VectorizedTo3DArray
#' 
#' Converts vectorized fluorescence dataframe into a 3-D array with Ex, Em, and
#' GRnumber as the dimensions. This results in one 2-D excitation-emmission array 
#' per sample.
#' 
#' @param df dataframe with vectorized fluorescence data in the format from the
#' CA WSC with the ExEm column along with one column per sample with GRnumber as
#' the header
#' @param ExEm Character name of column with Excitation and Emmission wavelengths
#' defined in this format: ###/###. For example Excitation 250 and emmission 400
#' would be represented as 250/400. 

#
# read vectorized fluorescence
# df <- read.csv("M:/QW Monitoring Team/GLRI toxics/Data Analysis/Optical/Final optical GLRI data/VectorizedFluorescenceCorrGLRI.csv",as.is=TRUE,skip=1)
# 
# aTest <- VectorizedTo3DArray(df)


VectorizedTo3DArray <- function(df,ExEm="Wavelength.Pairs"){
  library(reshape2)
  
  #separate the wavelength pairs so the "Ex" and "Em" wavelengths are in separate columns
  dfV <- cbind(read.table(textConnection(df$Wavelength.Pairs),sep="/"),df[,2:dim(df)[2]])
  colnames(dfV)[1] <- "Ex"
  colnames(dfV)[2] <- "Em"
  
  #Transform df to array
  m <- melt(data=dfV,id=c("Ex","Em"))
  colnames(m)[3] <- "GRnumber"
  
  #create 3-D array with Ex as 1st dimension, Ex as 2nd, and GRnum as 3rd
  a <- acast(m,Ex~Em~GRnumber)
  
  return(a)
}