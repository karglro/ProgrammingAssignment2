## Functions makeCacheMatrix and cacheSolve support the storage of an inverted matrix in a cache
## for quicker retrieval and to avoid multiple recomputations

## Function makeCacheMatrix
## Creates a wrapper of an invertible matrix, caching the inversion matrix for multiple retrievals
## input	an ordinary invertible matrix
##
## set	 	initializes the object with a new matrix
## get	 	retrieves the underlying matrix
## setinverse	sets the inverse matrix in cache
## getinverse	retrieves the stored inverse from cache


makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(mat) inverse <<- mat
  
  getinverse <- function() inverse
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Function cacheSolve
## Retrieves the inverted matrix from cache, if it exists. If not, it computes the inverted matrix and stores it in the cache.
## input	wrapper matrix (see: makeCacheMatrix)
## output       the inverted matrix of the underlying matrix of the wrapper

cacheSolve <- function(x, ...){
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}






