## This file implements the functions mackeCacheMatrix and cacheSolved which are used in combination to keep a cache of the 
## inverse of a matrix.
##
## Example usage:
## my_vect <- matrix(c(1, 2, 3, 1), 2, 2)  # create a matrix that we can calculate an inverse for
## my_special_vect <- makeCacheMatrix(my_vect)  # make a special "matrix" which supports caching the inverse
## cacheSolve(my_special_vect)  # calculate and cache the inverse
## cacheSolve(my_special_vect)  # same call as above, this time the cache of the inverse is found and used

## Returns a special "matrix" which can hold a cache of its inverse. The special "matrix" is actually
## a list that contains functions for getting and setting the internal matrix and the inverse, respectively.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  # The four functions used to get and set the internal matrix as well as the inverse
  set <- function(y) { x <<- y; inverse <<- NULL }  # Note that the cache is cleared when the internal matrix is set
  get <- function() { x }
  setinverse <- function(value) { inverse <<- value }
  getinverse <- function() { inverse }
  
  # Build and return the list of functions which make up the special "matrix"
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Returns the inverse for specified special "matrix", x.
## If a cached inverse is available, the cached value will be used without need for calculating it again. If a cached inverse is not
## available, it will be calculate and stored for later use.
cacheSolve <- function(specialMatrix, ...) {
  inverse <- specialMatrix$getinverse()
  
  # Use cached inverse if available
  if(!is.null(inverse)) {
    message("using cached data")
    return(inverse)
  }
  
  # No cached inverse was available, so inverse will be calculated and cached for later use.
  matrix <- specialMatrix$get()
  inverse <- solve(matrix)
  specialMatrix$setinverse(inverse)
  inverse
}
