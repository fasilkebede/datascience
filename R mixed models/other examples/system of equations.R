# System of equations

C <- matrix(c(1,2,1,2,-2,-1,-1,1,-1),nrow=3)
# C <- matrix(c(1,2,-1,2,-2,1,1,-1,-1),nrow=3, byrow=TRUE)

r <- matrix(c(4,2,1),nrow=3)

x <- solve(C) %*% r
            
            


