## This module provides functionality that allows calculating cached inverse of a matrix.

## This function returns an cached matrix object that can be used to calculate cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<-inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function returns the inverse of a given makeCacheMatrix's matrix by computing it 
## for the first time and returns the pre computed inverse if functioan is 
## executed for the same makeCacheMatrix again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}
