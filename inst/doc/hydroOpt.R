## ----openLibrary, echo=FALSE------------------------------
library(xtable)
options(continue=" ")
options(width=60)
library(knitr)


## ----include=TRUE ,echo=FALSE,eval=TRUE-------------------
opts_chunk$set(highlight=TRUE, tidy=TRUE, keep.space=TRUE, keep.blank.space=FALSE, keep.comment=TRUE, concordance=TRUE,tidy=FALSE,comment="")

knit_hooks$set(inline = function(x) {
   if (is.numeric(x)) round(x, 3)})
knit_hooks$set(crop = hook_pdfcrop)


## ----include=TRUE ,echo=FALSE,eval=TRUE-------------------
opts_chunk$set(highlight=TRUE, tidy=TRUE, keep.space=TRUE, keep.blank.space=FALSE, keep.comment=TRUE, tidy=FALSE,comment="")
knit_hooks$set(inline = function(x) {
   if (is.numeric(x)) round(x, 3)})
knit_hooks$set(crop = hook_pdfcrop)

## ----echo=FALSE-------------------------------------------
library("USGSHydroOpt")
GRnum <- "gr13307"
mat <- a[,,GRnum]
Ex <- as.numeric(names(a[,1,1]))
Em <- as.numeric(names(a[1,,1]))
nlevels <- 50
Peaks <- ex_ems
peakCol <- "Peak"
peakEx <- "ExCA"
peakEm <- "EmCA"
mainTitle <- "Example EEMs Plot"
exampleEEMs <- plotEEMs(mat=mat,Ex=Ex,Em=Em,nlevels=nlevels,Peaks=Peaks,peakCol=peakCol,peakEx=peakEx,peakEm=peakEm,mainTitle=mainTitle, titleSize=1)

## ----dfabs, echo=FALSE------------------------------------
# require(xtable) don't need xtable for this...
head(dfabs[,c(1:4)])

## ----dfFluor, echo=FALSE----------------------------------
require(xtable)
head(dfFluor[,c(1:5)])

## ----dfsags, echo=FALSE-----------------------------------
require(xtable)
dfsags

## ----dfsummary, echo=FALSE--------------------------------
require(xtable)
head(dfsummary[,c(1,22,23,25,30,44,47)])

## ----colnames(dfsummary)[11:68], echo=FALSE---------------
require(xtable)
colnames(dfsummary)[11:68]

## ----ex_ems, echo=FALSE-----------------------------------
require(xtable)
ex_ems

## ----ratioSignals, echo=FALSE-----------------------------
require(xtable)
ratioSignals

## ----signals, echo=FALSE----------------------------------
require(xtable)
signals

## ----array, echo=TRUE, eval=TRUE,tidy=TRUE----------------
#this command shows the excitation wavelengths (nm)
colnames(a) 

#this command shows the emission wavelengths (nm)
rownames(a) 

#this command shows the emission wavelengths (nm), only the first 20 shown for simplicity
names(a[1,1,])[1:20] 

## ----produce array workflow, echo=TRUE, eval=FALSE,tidy=TRUE----
#  #set an arbitrary data frame (df) as dfFluor (the example fluorescence dataframe in USGSHydroOpt)
#  df <- dfFluor
#  
#  #define the column in dfFluor
#  ExEm <- "Wavelength.Pairs"
#  
#  #assign a variable grnum to the column in dataSummary with sample names
#  grnum <- "GRnumber"
#  
#  #run the VectorizedTo3DArray function from USGSHydroOpt that creates a 3-D EEMs array given a vectorized fluorescence dataframe
#  aTest <- VectorizedTo3DArray(df,ExEm, grnum)

## ----getAbs,echo=TRUE, eval=TRUE,tidy=TRUE----------------
#assign a variable dataAbs to the absorbance dataframe included in USGSHydroOpt
dataAbs <- dfabs

#define the column with the wavelengths in dataAbs
waveCol <- "wavelengths"

#define the wavelengths for which absorbance coefficients should be defined
wavs <- c(430,530,630,730)

#define which columns contain the samples 
colSubsetString <- "gr"

#assign a variable dataSummary to the dfsummary dataframe included in USGSHydroOpt
dataSummary <- dfsummary

#assign a variable grnum to the column in dataSummary with sample names
grnum <- "GRnumber"

#use getAbs to produce absorbance coefficients 
testAbs <- getAbs(dataAbs,waveCol,wavs, colSubsetString,dataSummary,grnum)

#note that the absorbance coefficients as defined by wavs have been added to dataSummary
colnames(testAbs)[69:72]

## ----getSag, echo=TRUE,eval=FALSE, tidy=TRUE--------------
#  #assign a variable dataAbs to a shortened version of the absorbance dataframe included in USGSHydroOpt
#  dataAbs <- dfabs
#  
#  #define the column with the wavelengths in dataAbs
#  waveCol <- "wavelengths"
#  
#  #define the dataframe with the upper and lower wavelengths and name of spectral slope
#  #the example dataframe in USGSHydroOpt with this info is called 'dfsags'
#  sag <- dfsags
#  
#  #define which columns contain the samples
#  colSubsetString <- "gr"
#  
#  #assign a variable dataSummary to the dfsummary dataframe included in USGSHydroOpt
#  dataSummary <- dfsummary
#  sags <- grep('Sag',colnames(dataSummary))
#  
#  #remove columns with spectral slopes and re-compute with getSag
#  dataSummary <- dataSummary[,-c(sags)]
#  
#  #assign a variable grnum to the column in dataSummary with sample names
#  grnum <- "GRnumber"
#  
#  #use getSag to compute spectral slopes and add slopes to optical summary dataframe
#  testSag <- getSag(dataAbs,waveCol,sag,colSubsetString,dataSummary,grnum)
#  
#  #note that the spectral slopes defined in sag have been added to dataSummary
#  colnames(testSag)[65:68]

## ----getExpResid, echo=TRUE,eval=FALSE, tidy=TRUE---------
#  #absorbance wavelength (nm) for which residual is calculated
#  wavelength <- 267
#  
#  #the absorbance wavelength range (nm) to be considered as a numeric string
#  rangeReg <- c(240,340)
#  
#  #the absorbance wavelength range (nm) to be considered as a numeric string
#  rangeGap <- c(255,300)
#  
#  #assign a variable dataAbs to a shortened version of the absorbance dataframe included in USGSHydroOpt
#  dataAbs <- dfabs
#  
#  #define the column with the wavelengths in dataAbs
#  waveCol <- "wavelengths"
#  
#  #define which columns contain the samples
#  colSubsetString <- "gr"
#  
#  #assign a variable dataSummary to the dfsummary dataframe included in USGSHydroOpt
#  #column 68 or "Aresids" is removed because we are computing this summary optical variable
#  #with this function and then adding it to dataSummary
#  dataSummary <- dfsummary[,-c(68)]
#  
#  #assign a variable grnum to the column in dataSummary with sample names
#  grnum <- "GRnumber"
#  
#  #use getExpResid to calculate the residual at a given wavelength given a spectral slope calculated per Helms et al. 2008.
#  testdfOpt <- getExpResid(wavelength,rangeReg,rangeGap,dataAbs,waveCol,colSubsetString,dataSummary,grnum)
#  
#  #notice that the variable "Aresids" has been added to dataSummary
#  colnames(testdfOpt)

## ----getIndexes, echo=TRUE,eval=TRUE, tidy=TRUE-----------
#set a variable called a as the example 3-D excitation emission array included with the package
a <- a

#assign a variable dataSummary to the dfsummary dataframe included in USGSHydroOpt
dataSummary <- dfsummary 

#remove those columns with the fluorescence and humic indices that we are computing here
dataSummary <- dataSummary[,-c(43:46)] 

#assign a variable grnum to the column in dataSummary with sample names
grnum <- "GRnumber"

#use getIndexes to compute the four humification and fluorescence indices 
testIndexes <- getIndexes(a,dataSummary,grnum)

#note that the four indices have been added to dataSummary
colnames(testIndexes)[65:68]

## ----getRatios, echo=TRUE, eval=TRUE, tidy=TRUE-----------
#assign a variable dataSummary to the dfsummary dataframe included in USGSHydroOpt
dataSummary <- dfsummary

#note the number of variables in dataSummary
length(colnames(dataSummary))

#pick out the absorbance peaks and spectral slopes to be used for calculating ratios
#these correspond to those with a 1 in the "keep" column.
sigs <- ratioSignals[which(ratioSignals[2]>0),1]

#assign a variable grnum to the column in dataSummary with sample names
grnum <- "GRnumber"

#use getRatios to calculate 65 different ratios of absorbance peaks and spectral slopes
test <- getRatios(dataSummary,sigs,grnum)

#notice that 65 ratios have been added to dataSummary
length(colnames(test))

#example of some ratios added
colnames(test)[69:75]

## ----getMeanFl, echo=TRUE, eval=TRUE, tidy=TRUE-----------
#set a variable a as the example 3-D excitation emission array included with the package
a <- a

#set a variable signals as the example signals dataframe 
signals <- signals

#note the length of the EEMs peaks to be computed
length(rownames(signals))

#identify the name of the column with the EEMs Peak names
Peak <- "Peak"

#identify column with the lower excitation wavelength in the excitation wavelength range
Ex1 <- "Ex1"

#identify column with the upper excitation wavelength in the excitation wavelength range
Ex2 <- "Ex2"

#identify column with the lower emission wavelength in the emission wavelength range
Em1 <- "Em1"

#identify column with the upper emission wavelength in the emission wavelength range
Em2 <- "Em2"

#assign a variable dataSummary to the dfsummary dataframe included in USGSHydroOpt
dataSummary <- dfsummary

#remove the variables in dataSummary that are going to be computed with testMeanFl
rm <- which(colnames(dataSummary) %in% signals$Peak)
dataSummary <- dataSummary[,-c(rm)]

#note the number of variables in dataSummary
length(colnames(dataSummary))

#assign a variable grnum to the column in dataSummary with sample names
grnum <- "GRnumber"

#use getMeanFl to compute the different EEMs signals and add them to the optical summary data frame
testMeanFl <- getMeanFl(a,signals,Peak,Ex1,Ex2,Em1,Em2,dataSummary,grnum)

#notice that signals have been added to dataSummary
length(colnames(testMeanFl))
length(rownames(signals)) + length(colnames(dataSummary))

## ----getLog10, echo=TRUE, eval=TRUE, tidy=TRUE------------
#select those variables from ratioSignals which should be log 10 transformmed in dataSummary
signals <- ratioSignals[which(ratioSignals[2]>0),1]

#note the number of variables to be log 10 transformed
length(signals)

#assign a variable dataSummary to the dfsummary dataframe included in USGSHydroOpt
dataSummary <- dfsummary

#note the number of variables in dataSummary
length(colnames(dataSummary))

#assign a variable grnum to the column in dataSummary with sample names
grnum<-"GRnumber"

#use getLog10 to log 10 transform signals in dataSummary
testLog10 <- getLog10(dataSummary,signals,grnum)

#note that the log 10 transformed signals have been added to dataSummary
length(colnames(testLog10))
length(signals) + length(colnames(dataSummary))

## ----plotEEMs2,eval=TRUE,echo=TRUE,tidy=TRUE--------------
#choose a sample of interest from the 3-D EEMs array 
GRnum <- c("gr13307")

#create a matrix from the 3-D EEMs array with only data for sample GRnum
mat <- a[,,GRnum]

#define the excitation wavelengths (nm) as Ex data type numeric
Ex <- as.numeric(names(a[,1,1]))

#define the emission wavelengths (nm) as Em data type numeric
Em <- as.numeric(names(a[1,,1]))

#define the number of numeric color levels for contour plot
nlevels <- 50

#set a variable Peaks as the example dataframe ex_ems
Peaks <- ex_ems

#define the column in Peaks which contains the name of EEMs peaks
peakCol <- "Peak"

#define the column in Peaks which contains the excitation wavelength for a given Peak
peakEx <- "ExCA"

#define the column in Peaks whcih contains the emission wavelength for a given Peak
peakEm <- "EmCA"

#assign the main plot title
mainTitle <- paste("EEM spectra for",GRnum)

#use  to create a contour plot of EEM spectra
exampleEEMs2 <- plotEEMs(mat=mat,Ex=Ex,Em=Em,nlevels=nlevels,Peaks=Peaks,peakCol=peakCol,peakEx=peakEx,peakEm=peakEm,mainTitle=mainTitle, titleSize=1)

