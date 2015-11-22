## Sorry for my inglish 

## This file create function for cahce inverse matrix
## Example:
##   testMatrix <- matrix(1:4,2,2)
##   cacheData  <- makeCacheMatrix(testMatrix)
##   cacheSolve(cacheData) 
## ! print result first time
##   cacheSolve(cacheData) 
## ! Must print "getting cached data" and print result from cache


## Function makeCacheMatrix create list some function
## 
## @param x matrix data for cache inverse
## @return List(
##   set set matrix to cahche
##   get return matrix from cahce 
##   setSolve set inverce matrix to cahche
##   getSolve return result from cache
## )
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}

## Function for calculate inverse matrix with makeCacheMatrix function
## @param x makeCacheMatrix function with data
## @param ... other args for "solve" function
## @return solve matrix first and cache solve oter time
cacheSolve <- function(x, ...) {
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}
