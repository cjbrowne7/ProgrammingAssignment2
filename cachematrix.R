## Functions for inverting a matrix with caching.

## makeCacheMatrix creates a specialised matrix with in-built caching for solve() results.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) inverse <<- solve
  getsolve <- function() inverse
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## cacheSolve solves a matrix (i.e., returns its inverse).
## If the same matrix was previously solved, a cached value is returned.
cacheSolve <- function(x) {
  inverse <- x$getsolve()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  inverse <- solve(x$get())
  x$setsolve(inverse)
  inverse
}
