## This function will create a "matrix" object that
## will store a matrix and its inverse, and can set 
## or retrieve the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(y) inv <<- y
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}

## This function retrieves the inverse matrix
## from a "makeCacheMatrix" object. It will
## return the previously cached value, if it
## exists, or calculate and cache the value if
## not.

cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      message("calculating ...")
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}
