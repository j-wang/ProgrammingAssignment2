## Functions that create and operate "objects/classes" that enhance normal matrices in R
## by creating a cache functionality for matrix inverse. This saves computation time if 
## the matrix inverse is used multiple times (since it can then just be retrieved instead)
## of calculated from scratch.

## makeCacheMatrix: Object/"Class" with set, get, setinverse, and getinverse functions.
## set: sets matrix
## get: retrieves matrix
## setinverse: sets matrix inverse
## getinverse: gets cached matrix inverse (returns null if not previously calculated)
##
## (This functionality is enabled by R's closures and first class functions.)

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: Takes a CacheMatrix object (created by makeCacheMatrix)
## returns calculated inverse from cache if previously computed,
## otherwise computes inverse of matrix, caches it, and returns it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
