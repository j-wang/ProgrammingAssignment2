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
  
  # SET: sets the underlying matrix
  set <- function(y) {
    x <<- y
    i <<- NULL  # reset i to NULL, since we don't have new matrix's inverse
  }
  
  # GET: return underlying matrix
  get <- function() x
  
  # SETINVERSE: set cached inverse
  setinverse <- function(inv) i <<- inv
  
  # GETINVERSE: returns cached inverse
  getinverse <- function() i
  
  # return list of class "methods" (for those who know OOP) / functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: Takes a CacheMatrix object (created by makeCacheMatrix)
## returns calculated inverse from cache if previously computed,
## otherwise computes inverse of matrix, caches it, and returns it.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  
  # if there is a cached solution, return it
  if(!is.null(i)) {
    message("cached data available:")
    return(i)
  }
  
  # otherwise, get the underlying matrix and solve for inverse
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)  # cache the data so we can retrieve it next time
  i  # return inverse
}
