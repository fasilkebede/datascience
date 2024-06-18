##########################################################################################
##### Genomic Selection with rrBLUP R package #######################################################
####Three traits i.e. body weight, body length and heigh used#############################
#Author: Fasil Getachew, 2024

#load in the sample files
library(rrBLUP)
#setwd("C:/A/Fasil/rrBLUP")

rm(list = ls())
#load the Markers and Phenotypes

Markers <- as.matrix(read.table(file="snp.txt"), header=F) #i.e markers do not have names
head(Markers)
str(Markers) # 1 = homozaygous for parent 1, 0=heterozygous, and -1 homozygous for parent 2; i.e. marker in {-1,0,1} format for rrBLUP
# Do not put headers on the market matrix, other wise it cannot multiply and solve
# NAs need to be imputed; no missin markers
# The number of individuals in the phenotype matrix should be equal to the individuals with markers

Pheno <-as.matrix(read.table(file ="traits.txt", header=TRUE)) # phenotype has header #phenotype not as.character
head(Pheno)
#dimensions of the matrix
dim(Markers) #96 individuals (rows); 1178 markers(columns)
dim(Pheno) #96 indi. (rows); 3 traits (columns)
#
sum(is.na(Markers))
which(is.na(Markers))
#####
#what if markers are NA?

#********************######
#Impute with A.mat()#######
###########################
impute=A.mat(Markers,max.missing=0.5,impute.method="mean",return.imputed=T) #imputed value to be replaced is the mean #A.mat also estimates relatinship matrix
Markers_impute=impute$imputed #rename  imputed  marker matrix as Markers_impute
impute$imputed #returns teh imputed marker matrix
head(Markers_impute)
dim(Markers_impute) #two less columns, i.e. two markers need to be removed (>50% missing data); are columns 169 and 562; no missing data allowed in rrBLUP
#impute$A #returns the additive relationship matrix; proportion of genes identical by descent, expected to be shared by two animals

#remove markers with more than 50% missing data
Markers_impute2=Markers_impute[,-c(169,562)]
dim(Markers_impute)
dim(Markers_impute2) #two less individuals
#####

#######**********************************#######
#Define the training and test populations#######
#****************************************#######

#training-60%; validation-40%
# Validation population-phenotype values estimated based on marker effects calculated from training population
# 58 (60% of total population of 96) random numbers sampled to determine which individuals are in the training population
#Individuals are the row numbers for the phenotypes and marker matrices; Sampled numbers will be different every time the code is run and will affect the correlation accuracy

train= as.matrix(sample(1:96, 38)) #genotyped and phenotyped (96*0.4 = 38 individuals to be sample each time it is run)
test<-setdiff(1:96,train) #also called validation population, is only genotyped; 
# setdiff() determines the numbers that are not in the training population and will be part of the validation population

Pheno_train=Pheno[train,] #the phenotype matrix for the training population
m_train=Markers_impute2[train,] #the marker matrix for the training population
Pheno_valid=Pheno[test,] #the phenotype of the validation popu.
m_valid=Markers_impute2[test,] #the marker of the validation popu.

#########******************************************************################
########Run mixed.solve() and determine accuracy of predictions################
#########******************************************************################

# Pheno_train in mixed.solve is the Nx1 vector of phenotype means of the training population
# $Beta in mixed.solve is the overall mean of the training set i.e. meu
# m_train is the NxNM (marker matrix)
#$u in mixec.solve represents the Nm X 1 (marker effects matrix)
#Nx1 vector of residual effect

yield=(Pheno_train[,1]) #Yield is the first column of the pheno_train matrix
yield_answer<-mixed.solve(yield, Z=m_train, K=NULL, SE = FALSE, return.Hinv=FALSE) #Z is the design matrix of random effects; K is the identity matrix; standard errors are not calculated
YLD = yield_answer$u #Yield_answer$u is the output of the marker effects 
e = as.matrix(YLD) #these effects of each of 1174 markers on individuals from the training population

pred_yield_valid =  m_valid %*% e #marker validation matrix times the marker effects
pred_yield=(pred_yield_valid[,1])+yield_answer$beta #predicted yield(bwt) of the 58 individuals in our validation population based on the marker effects of the training population with the grand mean added in
yield_valid = Pheno_valid[,1]

###**********************************##########
#Determine the correlation accuracy############
#**********************************############

#change slightly each time the code is run due to different individuals sampled for the training and validation population
# Accuracy is affectd by affected by a) training size, b) validation size, c) number of markers and heritability

YLD_accuracy <-cor(pred_yield_valid, yield_valid, use="complete" ) # Correlation between the predicted yield values and the observed phenotype (yield) values
YLD_accuracy 

#Now let's do the same for the second trait i.e. body length;correlation accuracy is different for each trait
PHT_HT=(Pheno_train[,2])
PHT_HT_answer<-mixed.solve(PHT_HT, Z=m_train, K=NULL, SE = FALSE, return.Hinv=FALSE)
PHT_HT = PHT_HT_answer$u
e = as.matrix(PHT_HT)
pred_PHT_HT_valid =  m_valid %*% e
PHT_HT_valid = Pheno_valid[,2]
PHT_HT_accuracy <-cor(pred_PHT_HT_valid, PHT_HT_valid, use="complete" )
PHT_HT_accuracy 

#  And now for height
HD_DATE=(Pheno_train[,3])
HD_DATE_answer<-mixed.solve(HD_DATE, Z=m_train, SE = FALSE, return.Hinv=FALSE)
HD_DATE = HD_DATE_answer$u
e = as.matrix(HD_DATE)
pred_HD_DATE_valid =  m_valid %*% e
HD_DATE_valid = Pheno_valid[,3]
HD_DATE_accuracy <-cor(pred_HD_DATE_valid, HD_DATE_valid, use="complete" )
HD_DATE_accuracy 


#### cross validation for many cycles for yield only
traits=1
cycles=500
accuracy = matrix(nrow=cycles, ncol=traits)
for(r in 1:cycles)
{
  train= as.matrix(sample(1:96, 29))
  test<-setdiff(1:96,train)
  Pheno_train=Pheno[train,]
  m_train=Markers_impute2[train,]
  Pheno_valid=Pheno[test,]
  m_valid=Markers_impute2[test,]
  
  yield=(Pheno_train[,1])
  yield_answer<-mixed.solve(yield, Z=m_train, K=NULL, SE = FALSE, return.Hinv=FALSE)
  YLD = yield_answer$u
  e = as.matrix(YLD)
  pred_yield_valid =  m_valid %*% e
  pred_yield=(pred_yield_valid[,1])+yield_answer$beta
  pred_yield
  yield_valid = Pheno_valid[,1]
  accuracy[r,1] <-cor(pred_yield_valid, yield_valid, use="complete" )
}
mean(accuracy)

