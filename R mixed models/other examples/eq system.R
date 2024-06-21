# System of equations

# A x = c

A<-matrix(c(3, 4, 4, 6,
            9, 2, -1, -6,
            1, 1, 1, -10,
            2, 9, 2, -1),nrow=4)
            
c<-matrix(c(-10, 20, 2, -10),nrow=4)

#solution
x <- solve(A) %*% c
x
