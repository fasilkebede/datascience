# Example
# QTL affecting a specific quantitative trait

# y = Xb + e

# vector of phenotypes
y<-matrix(c(95.9, 108.0, 96.5, 92.9, 101.0, 94.5, 93.7, 89.8,
            101.2, 103.9, 85.9, 109.4, 105.7, 98.4, 84.1, 103.1,
            117.1, 95.2, 106.4, 104.7, 92.5, 123.9, 97.8, 100.5),nrow=24)

# vector with genotypes
X<-matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 
            -1, -1, -1, -1, -1, -1, -1, -1, 
            0, 0, 0, 0, 0, 0, 0, 0, 
            1, 1, 1, 1, 1, 1, 1, 1),nrow=24)

#solution
b <- solve(t(X) %*% X) %*% t(X) %*% y

# Using package lm

qtl <- X[,2]
reg <- lm(y ~ qtl)
summary(reg)
