#########################################################################################################################################
#################rrBLUP pipeline for genomic prediction##################################################################################
#################**************************************##################################################################################
#rrBLUP package https://cran.r-project.org/web/packages/rrBLUP/rrBLUP.pdf
#uses ridge regression BLUP for genomic predictions #https://www.ncss.com/wp-content/themes/ncss/pdf/Procedures/NCSS/Ridge_Regression.pdf
#I use a two step approach: which is computationally more efficient compared to one step
#In two step approach, adjusted means are calculated across locations, and then means are then used in rigression blup
###############################
##############################
# A) This pipeline shows
#1.prediction of marker effects through mixed.solve()
#2.Imputng missing markers with A.mat() (i.e. Mixed.solve does not allow NA marker values
#3.Define the training and validation population
# A.mat calculates the additive relationship matrix
##***************************************************************************#############################################################
##########################################################################################################################################
#install.packages("rrBLUP")
rm(list = ls())

setwd("C:/Users/FGetachew/jupyter_workspace/rrBLUP")

library(rrBLUP)
#
#Define the training and test (validation) populations
train <- read.csv('animal.train.nan-101.csv')
head(train)
dim(train)
test <- read.csv('animal.test.nan-101.csv')
head(test)
dim(test)
train_x <- as.matrix(train[-1])
train_y <- as.matrix(train['label'])
test_x <- as.matrix(test[-1])
test_y <- as.matrix(test['label'])


# impute missing markers using A.mat()
# A.mat
start_time <- Sys.time()
Amat <- A.mat(train_x)
print(Amat)
print(Sys.time()-start_time)

# EM impute method
start_time <- Sys.time()
Amat <- A.mat(train_x, impute.method='EM')
print(Amat)
print(Sys.time()-start_time)

# mean impute method, return imputed 
#max.missing-maximum proportion of missing data 50% (i.e. if >= 50%, then the markers will not be imputed and they get removed)
#return.imputed prints out the imputed results if set to TRUE
start_time <- Sys.time()
result <- A.mat(train_x,max.missing = 0.5, impute.method='mean', return.imputed=TRUE)

#result$A returns the additive relationship matrix
Amat <- result$A

#result$imputed returns the imputed market matrix
train_x_imp <- result$imputed
print(Amat)
print(train_x_imp)
print(Sys.time()-start_time)

# EJ shrink method
start_time <- Sys.time()
Amat <- A.mat(train_x, shrink=TRUE)
print(Amat)
print(Sys.time()-start_time)

# REG shrink method
start_time <- Sys.time()
Amat <- A.mat(train_x, shrink=list(method="REG",n.qtl=100,n.iter=5))
print(Amat)
print(Sys.time()-start_time)


train <- read.csv('data/protein.train.csv')
test <- read.csv('data/protein.test.csv')

train_x <- as.matrix(train[-1])
train_y <- as.matrix(train['label'])
test_x <- as.matrix(test[-1])
test_y <- as.matrix(test['label'])

# mixed.solve using Matrix Z
# Run mixed.solve () and determine accuracy of predictions
start_time <- Sys.time()
result <- mixed.solve(y=train_y, Z=train_x)
print(result)
print(Sys.time()-start_time)
test_pred <- (test_x %*% as.matrix(result$u)) + as.vector(result$beta)
print(test_y)
print(test_pred)

# mixed.solve using Matrix K
start_time <- Sys.time()
result <- mixed.solve(y=train_y, K=A.mat(train_x))
print(result)
print(Sys.time()-start_time)
train_pred <- as.matrix(result$u) + as.vector(result$beta)
print(train_y)
print(train_pred)