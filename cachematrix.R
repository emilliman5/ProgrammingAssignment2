## This function creates and caches a matrix with dedicated methods (get, set, inverse, getinverse).
## this function also solves the inveres of an invertible matrix and stores the solution for later use

## This is a function to cache a matrix (assumed to be invertible)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  inverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       inverse = inverse,
       getinverse = getinverse)
}

## This is a function to solve and cache the inverse of the matrix created by the above function matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$inverse(m)
  m
}