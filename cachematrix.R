makeCache <- function(x = matrix()) {
## MAKECACHE() returns a list of functions that 
## save or draw the input matrix and its inverse (cache)
## list$set() saves a new matrix to input variable
## list$get() returns the input matrix
## list$setInverse() saves the inverse to cache
## list$getInverse() returns the inverse from cache
  invMat <- NULL
  set <- function(y) {
    x <<- y
    invMat <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) invMat <<- Inverse
  getInverse <- function() invMat
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


cacheInverse <- function(xlist) {
## CACHEINVERSE() returns the inverse matrix of 'x' through the saved cache
## Input the list obtained from the above function
  # check the cache to return
  invMat <- xlist$getInverse()
  if(!is.null(invMat)) {
    message("getting cached data")
    return(invMat)
  }
  # if the cache is empty, calculate the inverse, 
  # save to the cache, and then return
  data <- xlist$get()
  invMat <- solve(data)
  xlist$setInverse(invMat)
  invMat
}
