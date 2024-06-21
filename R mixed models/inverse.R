# Matrix inverse

A <- matrix(c(1,-1,2,0), nrow=2)

B <- matrix(c(1,2,2,4), nrow=2) # singular matrix

detA=det(A)
Ainv <- solve(A)

detB=det(B)
Binv <- solve(B)

C <- matrix(c(1,1,2,2,1,1,0,0,1), nrow=3) 

Cinv <- solve(C)   
            


