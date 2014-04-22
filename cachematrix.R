## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
