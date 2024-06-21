# Example with candidate gene

# vector of phenotypes
y<-matrix(c(3.4,3.7,3.2,2.9,2.5,3.1,2.6),nrow=7) 

# incidence matrix (genotypes)
X<-matrix(c(1,1,1,1,1,1,1,-1,-1,-1,0,0,1,1,0,0,0,1,1,0,0),nrow=7) 


#Matrix calculations
XX<-crossprod(X,X)
Xy<-t(X) %*% y

b <- solve(XX) %*% Xy
b
