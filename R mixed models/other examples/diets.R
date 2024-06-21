# Example with Diets (A, B and C)

# response vector

y<-matrix(c(106,99,97,104,99,105,95,84,99,89,80,82,92,99,85,91,89,92),nrow=18) 
# incidence fixed effects (diets)
X<-matrix(c(1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,
            0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,
            0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1),nrow=18) 
            
diet<-lm(y~X - 1)
summary(diet)

#Analysis of Variance
fit <- aov(y ~ X)

layout(matrix(c(1,2,3,4),2,2)) # optional layout
plot(fit) # diagnostic plots

summary(fit) # display Type I ANOVA table


#Matrix calculations
XX<-crossprod(X,X)
Xy<-t(X) %*% y

b <- solve(XX) %*% Xy
b

SST<-t(y)%*%y-nrow(y)*mean(y)^2
SSD<-t(b)%*%Xy-nrow(y)*mean(y)^2
#SSR<-SST-SSD
SSR<-t(y-X%*%b)%*%(y-X%*%b)
df1<-ncol(X)-1
df2<-nrow(X)-ncol(X)
F<-(SSD/df1)/(SSR/df2)
F

p<-1-pf(F, df1, df2)
p