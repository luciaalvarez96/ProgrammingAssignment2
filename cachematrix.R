## In this assignment we'll write a pair of functions that cache the inverse of a matrix
## (assuming that the matrix supplied is always invertible)
## i.e. makeCacheMatrix and cacheSolve 

## The function makeCacheMatrix will be used to create a special "matrix" object that can
## cache its inverse.

makeCacheMatrix <- function(a = matrix()) {
  mymat <- NULL
  setm <- function(b) {
    a <<- b
    mymat <<- NULL
  }
  getm <- function() a
  setinv <- function(inverse) mymat <<- inverse
  getinv <- function() mymat
  list(setm = setm , getm = getm,
       setinv = setinv,
       getinv = getinv)
}


## The function cacheSolve computes the inverse of the special "matrix" returned by the
## previous function. If the inverse has already been calculated (and the matrix has not
## changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(a, ...) {
  ## Return a matrix that is the inverse of 'x'
        mymat <- a$getinv()
        if (!is.null(mymat)) {
          message("getting cached data")
          return(mymat)
        }
        datamat <- a$getm()
        mymat <- solve(datamat, ...)
        a$setinv(mymat)
        mymat
}

## Testing the functions
## mymat <- matrix(c(1,0,2,-1,1,0,0,0,1),3,3)
## mymat1 <- makeCacheMatrix(mymat)
## cacheSolve(mymat1)
##
## mymat <- matrix(c(1,1,1,3,4,3,3,3,4),3,3)
## mymat1 <- makeCacheMatrix(mymat)
## cacheSolve(mymat1)
