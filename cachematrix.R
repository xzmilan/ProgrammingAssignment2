##MY CODE FOR CACHING THE INVERSE OF A MATRIX AND 
#FUNCTION TO CREATE MATRIX AND ITS INVERSE FOR A SQUARE 2X2
makeCacheMatrix <- function(x = matrix()) {
  Mtx<-NULL
  MtxID <<- diag(nrow = 2, ncol = 2)
    set<-function(y){
    x <<- y
    Mtx <<- NULL
  }
  get<- function() x 
  setMtxInverse <-function(inverse) Mtx<<- inverse
  getMtxInverse<-function() Mtx
  list(set=set, get=get,
       setMtxInverse=setMtxInverse,
       getMtxInverse=getMtxInverse)
}
#Retrieving the Matrix, and setting Inverse 
cacheSolve <- function(x, ...) {
  Mtx<-x$getMtxInverse()
  if(!is.null(Mtx)){
    print("retrieving cached data")
    return(Mtx)
  }
  
  Mtx<-solve(x$get())
  x$setMtxInverse(Mtx)
  Mtx
}

#Setting Matrix variables, and calculating Inverse, and test against Identity Matrix
a <- makeCacheMatrix(matrix(1:4,2))
#Square Matrix  
a$get()
#Inverse of Square Martrix
cacheSolve(a)
#Test Against Identity Matrix 
MtxID
#Production of Identity Matrix from Current Square Matrix "*" Inverse 
a$get()%*%a$getMtxInverse()


