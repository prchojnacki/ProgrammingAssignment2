## These two function cache the inverse of a matrix so
## that it doesn't have to be recalculated multiple times

## This function creates a matrix object that can cache
## its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
         x <<- y
         inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## This function computes the inverse of the matrix
## returned by makeCacheMatrix above.  If the inverse has
## already been calculated, this function will retrieve
## it from the cache.

cacheSolve <- function(x, ...) {
     inv <- x$getinv()
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     data <- x$get()
     ## Return a matrix that is the inverse of 'x'
     inv <- solve(data, ...)
     x$setinv(inv)
     inv
}