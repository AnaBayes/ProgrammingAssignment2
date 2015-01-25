## Functions makeCacheMatrix and cacheSolve cache the invers of a matrix

## The function makeCacheMatrix creates a special "matrix" object that:
## 1. sets and gets the value of the matrix
## 2. sets and gets the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL
  set <- function() x # set is a function that returns the matrix x
  setinverse<-function(inverse) x_inv <<-inverse #?? 
  getinverse<-function() x_inv # function that returns matrix inverse
  list (set =set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## The function catchSolve calculates the inverse of the matrix created by the above 
## function.First it checks if the inverse has already been calculated, if that is the case,
## it takes the inverse from the list and skips the computation. Otherwise, it calculates 
## the inverse of the matrix and puts it in the catche via the setinverse function

cacheSolve <- function(x, ...) {
    x_inv <- x$getinverse() 
    if(!is.null(x_inv)){ 
      message ("getting cached inverse")
      return(x_inv)
    } 
    data <- x$get() 
    x_inv <- solve(data) 
    x$setinverse(x_inv) 
    x_inv 
  }
