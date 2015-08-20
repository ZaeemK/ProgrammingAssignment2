## Matrix inversion is a costly computation and there may be come benefit to
## caching the inverse of a matrix rather than computing it repeatedly
## The two functions below are used to create an object that stroes a matrix
## and caches its inverse

## This function creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function computes the inverse of the matrix created by
## the function above. If the inverse has already been calculated,
## and the matrix has not changed, then this function should
## retrieve the inverse from the cache


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinverse(inv)
    inv
}
