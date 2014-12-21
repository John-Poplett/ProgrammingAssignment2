##
## This source code module defines two R functions makeCacheMatrix and cacheSolve.
## These methods make it possible to cache the results from inverting a matrix
## to avoid redundant and computationally expensive calls to a solver.
##

##
## makeCacheMatrix creates a closure around a matrix variable "x" and a secondary
## variable "inverse" that caches the inverse of the matrix. makeCacheMatrix, as its
## name implies, is a constructor method. It returns a list of setter and getter
## methods that provide controlled access to the closure's variables.
##
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    # No need to recompute the inverse if x and y are identical
    if(!identical(x,y)) {
      x <<- y
      inverse <<- NULL
    }
  }
  get <- function() x
  setinverse <- function(inverse) inverse <<- inverse
  getinverse <- function() inverse
  list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}

##
## cacheSolve returns the inverse of the matrix X which is a closure
## created by the makeCacheMatrix method. Whenever possible, it will
## return a cached copy of the inverted matrix.
##
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}