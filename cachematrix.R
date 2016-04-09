## These functions create and store a matrix and its inverse for faster access,
## without the need to recalculate the inverse of the given matrix

## This function creates a "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  
  invM <- NULL
  
  set <- function(y) {
    x <<- y
    invM <<- NULL
  }
  get <- function() x
  setInvM <- function(matr) invM <<- matr
  getInvM <- function() invM
  
  list(set=set, get=get, setInvM=setInvM, getInvM=getInvM)
}

## This function computes the inverse of the "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve should retrieve the inverse from
## the cache.

cacheSolve <- function(x, ...) {
  invM <- x$getInvM()
  if(!is.null(invM)) {
    message("getting cached data")
    return(invM)
  }
  data <- x$get()
  invM <- solve(data)
  x$setInvM(invM)
  invM
}
