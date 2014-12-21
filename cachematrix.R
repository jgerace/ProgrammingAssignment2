## Functions to allow users to calculate the inverse of a matrix.
## This inverse will remain cached to reduce the number of computations
## needed to get the inverse of the same exact matrix.

## makeCacheMatrix()
##  Input: an invertible matrix
##  Output: list of functions that will allow you to find the inverse
##          of the matrix you passed in
##
## cacheSolve()
##  Input: the list of functions returned by makeCacheMatrix()
##  Output: the inverse of the matrix passed into makeCacheMatrix()

## Usage:
## x <- matrix(1:4, 2, 2) ## Create a 2x2 matrix
## a <- makeCacheMatrix(x) ## Get the special "matrix", which is really just a list of functions
## cacheSolve(a) ## Get the inverse of the "matrix". This also caches the inverse.
## cacheSolve(a) ## Get the inverse again, but this time it should say it's returning the cached data.


## Accepts an invertible matrix and returns a list of matrix functions
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## Accepts a list of matrix functions returned by makeCacheMatrix() and
## returns the inverse of the matrix given to makeCacheMatrix(). Will either
## find the inverse of the matrix or will return the cached inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
