# Generalized inverse

library(Matrix)

A <- matrix(c(2,4,1,1,2,2,0,1,0,2,1,0), nrow=4) 

r <- rankMatrix(A)[1]

M <- A[2:3,2:3]
Mit <- t(solve(M)) 

Agi <- matrix(0, 4, 3)
Agi[2:3,2:3] <- Mit
Agi <- t(Agi)

# test
A %*% Agi %*% A
            


