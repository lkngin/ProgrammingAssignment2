## Put comments here that give an overall description of what your
## functions do
## A pair of functions that cache the inverse of a matrix.

## Write a short comment describing this function
## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(mtx = matrix()) {
  inverse <- NULL
  set <- function(x) {
    #  use '<<-' ti assign a value to an object in an environment different form the current environment.
    mtx <<- x
    inverse <<- NULL
  }
  get <- function() return(mtx)
  setinv <- function(inv) inverse <<- inv
  getinv <- function() return(inverse)
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(mtx, ...) {
  ## Return a matrix that is the inverse of 'mtx'
  inverse <- mtx$getinv()
  
  # if the inverse has already been calculated, get from the cache
  if(!is.null(inverse)) {
    message("getting cached data...")
    return(inverse)
  }
  
  # else, calculates the inverse
  data <- mtx$get()
  invserse <- solve(data, ...)

  # set the value of the inverse in the cache
  mtx$setinv(inverse)
  
  return(inverse)
}
