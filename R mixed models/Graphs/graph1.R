## To install Rgraphviz
## try http:// if https:// URLs are not supported
#source("https://bioconductor.org/biocLite.R")
#biocLite("Rgraphviz")

# Population in HW
# 1 dialellic locus and 2 traits
# Locus affects directly only trait 1, trait 1 affects trait 2
par(mfrow=c(1,1))
library(Rgraphviz)
net<-rbind(c(0,1,0), c(0,0,1), c(0,0,0))
rownames(net)<-c("G", "Y1", "Y2")
colnames(net)<-c("G", "Y1", "Y2")
netgraph<-new("graphAM", adjMat=net, edgemode="directed")
plot(netgraph, attrs= list(node=list(fillcolor="green"), edge=list(arrowsize=0.8, lwd = 4)))

set.seed(8949)

# Generation genotypes: aa=-1, Aa=0, and AA=1
n=400 # sample size
p=0.6 # p = Pr(A)
geno=rep(-1, times=n)+rbinom(n, 1, p)+rbinom(n, 1, p)

# Trait 1
mu1=100 # intercept
b1=5
s1=3 # residual standard deviation
y1<-mu1+geno*b1+rnorm(n, 0, s1)
#hist(y1)

# Trait 2
mu2=50 # intercept
lambda=1 # additive genetic effect
s2=2 # residual standard deviation
y2<-mu2+lambda*y1+rnorm(n, 0, s2)
#hist(y2)

#data set
id<-seq(1, n, by=1)
data<-cbind(id,geno, y1, y2)

data<-as.data.frame(data)

#write.table(data, "/Users/Rosa/Dropbox/2014/teaching/Synbreed/Lab/Examples/data.txt", sep="\t") 

plot(geno,y1)
plot(geno,y2)
plot(y1,y2)

#regression
fit1<-lm(y2~1+geno, data=data)
summary(fit1)

fit2<-lm(y2~1+geno+y1, data=data)
summary(fit2)

fit3<-lm(y1~1+geno, data=data)
summary(fit3)

fit4<-lm(y1~1+geno+y2, data=data)
summary(fit4)

cor(fitted(fit3), data$y1)
cor(fitted(fit4), data$y1)
