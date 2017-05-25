## This module provides functionality that allows calculating cached inverse of a matrix.

## This function returns an cached matrix object that can be used to calculate cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) minv <<-inv
  getinv <- function() minv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function returns the inverse of a given makeCacheMatrix's matrix by computing it 
## for the first time and returns the pre computed inverse if functioan is 
## executed for the same makeCacheMatrix again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mtx <- x$getinv()
  if(!is.null(mtx)) {
    message("getting cached data")
    return(mtx)
  }
  data <- x$get()
  mtx <- solve(data)
  x$setinv(mtx)
  mtx
}
