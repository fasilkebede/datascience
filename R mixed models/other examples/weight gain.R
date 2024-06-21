# Regression of daily weight gain on diet protein

data<-read.table('/Users/guilherme/Desktop/lab/examples/weight gain.csv',sep=',',header=TRUE)

cp<-as.matrix(data[,1])
dg<-as.matrix(data[,2])

reg<-lm(dg ~ cp)

reg
summary(reg)

cov(cp,dg)
var(cp)
cov(cp,dg)/var(cp)
cor(cp,dg)

# standardized variables

scaled.data <- scale(data)

z_cp<-as.matrix(scaled.data[,1])
z_dg<-as.matrix(scaled.data[,2])

reg_z<-lm(z_dg ~ z_cp)

reg_z
summary(reg_z)

cov(z_cp,z_dg)
var(z_cp)
cov(z_cp,z_dg)/var(z_cp)
cor(z_cp,z_dg)
