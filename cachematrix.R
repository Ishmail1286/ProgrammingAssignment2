## Setting and Inverting a matrix.
## The process of inverting the matrix "X" will occur when 
##  the inverse has not been calculated

## Function that makes the matrix, which consists of a list 
# containing a function to:
# 1.- Set the value of the matrix
# 2.- Get the value of the matrix
# 3.- Set the value of the inverse
# 4.- Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  set <- function(y){
    x<<-y
    inverse <<-NULL
  }
  get<-function()x
  setInverse<-function(inv) inverse<-inv
  getInverse <- function() inverse
  list(set=set, get=get, 
       setInverse=setinverse,
       getInverse=getInverse)

}


## cacheSolve, calculates the inverse of a matrix

cacheSolve <- function(x, ...) {
  inverse<-x$getInverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return (inverse)
  }
  matrix<-x$get()
  inverse<-solve(matrix,...)
  x$setInverse(inverse)
  inverse
  
}
