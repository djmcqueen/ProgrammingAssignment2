## These 2 functions create matrix objects that cache their inverse after
## it's calculated, to avoid needing to do so again on the same matrix.


## The makeCacheMatrix function creates the matrix object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) i <<- solve
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

## The cacheSolve function returns the inverse of the matrix object from
## makeCacheMatrix, using the cached inverse if it was already calculated,
## or calculating the inverse and storing it in the cache if it was not.

cacheSolve <- function(x, ...) {
## Returns a matrix that is the inverse of 'x'
  
  i <- x$getInverse()
  if(!is.null(i)) {
    message("Cached Inverse Found! Using Cached Object.")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  message("No Cached Inverse Found - Calculating and Caching Inverse.")
  i
  
}
