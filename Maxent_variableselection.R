############################################################################
########Step-3:Maxent variable selection
#Fasil Getachew Kebede
#November 2019


#install required packages
install.packages('maptools')
install.packages('MaxentVariableSelection')

#upload packages
library(maptools)
library(rgeos)
library(rgdal)
library(raster)
library(sp)
library(rJava)
library(MaxentVariableSelection)

##############################################################
#############################################################
#MaxentVariableSelection
#####################################################
#Summary prepared to run Maxent
outdir <- ("C:/MaxentVarSel/output_climsoil/")
maxent <- ("C:/MaxentVarSel/maxent.jar")
gridfolder<- ("C:/MaxentVarSel/gridData")
occurrencelocations<- "C:/MaxentVarSel/ValuesAtOccurrencelocations.csv"
backgroundlocations<- "C:/MaxentVarSel/ValuesAtBackgroundlocationsEthiopianChicken.csv"
additionalargs="nolinear noquadratic noproduct nothreshold noautofeature"
contributionthreshold<- 5
correlationthreshold <- 0.9
betamultiplier=seq(2,6,0.5)
VariableSelection(maxent,
                  outdir,
                  gridfolder,
                  occurrencelocations,
                  backgroundlocations,
                  additionalargs,
                  contributionthreshold,
                  correlationthreshold,
                  betamultiplier
                  )

