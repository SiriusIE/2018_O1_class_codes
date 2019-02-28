# let's study the diagonalization property
# of a square matrix

A<-matrix(c(1,0,2,
            2,3,-4,
            0,0,2),ncol=3)
A


eigenvalues<-eigen(A)[['values']] 
eigenvectors<-eigen(A)[['vectors']]

eigenvalues       # all values distinct so the matrix is diagonalizable

eigenvectors      # then P diagonalizes A: 


P<-eigenvectors
diagonalization<-solve(P)%*%A%*%P

lambda<-diag(eigenvalues) 

identical(diagonalization,lambda)

A_recovery<-P%*%lambda%*%solve(P)

identical(A,A_recovery)




