## 12/21/14, Coursera, R Programming, Week 3, Programming Assignment 2
## This pair of functions calculates and caches the inverse of a matrix.

## The makeCacheMatrix() function creates a special object that can
## that can store a matrix and also store the cached inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getx <- function() x
  setinv <- function(invtocache) inv <<- invtocache
  getinv <- function() inv
  list(set = set, getx = getx, setinv = setinv, getinv = getinv)
}

## The cacheSolve() function computes the inverse of a matrix using the
## special object created by the makeCacheMatrix() function. If the inverse
## of the matrix has already been calculated (and the matrix has not changed),
## then the cachesolve() function retrieves the cached inverse.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  inv <- solve(x$getx())
  x$setinv(inv)
  inv
}

