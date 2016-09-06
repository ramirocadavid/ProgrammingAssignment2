## In order to minimize computation time for matrices for which I have already calculated the inverse, I will
## create two functions that will allow me to store the results of the inverse of a matrix and return it anytime
## I need it, or calculate it if the matrix has changed.

## This function creates a list of functions (or a special "matrix") that can cache the
## inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
     x.inv <- NULL
     set <- function(y) {
          x <<- y
          x.inv <<- NULL
     }
     get <- function() x
     setInv <- function(inv) x.inv <<- inv
     getInv <- function() x.inv
     list(set = set, get = get,
          setInv = setInv, getInv = getInv)
}


## This function computes the inverse of the "matrix" returned by makeCacheMatrix. However, if the matrix has
## not changed and its inverse has been calculated, then the function returns the cached inverse, instead of
## calculating it again.

cacheSolve <- function(x, ...) {
     x.inv <- x$getInv()
     if(!is.null(x.inv)){
          message("getting cached inverse of x")
          return(x.inv)
     } else {
          data <- x$get()
          x.inv <- solve(data, ...)
          x$setInv(x.inv)
          x.inv
     }        ## Return a matrix that is the inverse of 'x'
}
