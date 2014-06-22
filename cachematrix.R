## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
## rather than computing it repeatedly. The following functions cache and calculate the inverse of a matrix.
##------------------------------------------------------------------------------------------------------------------

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
      	x <<- y
            inv <<- NULL
        }
	get <- function() {x}
	setinv <- function(inverse) {inv <<- inverse}
	getinv <- function() {inv}
      list(set = set, get = get,
      	setinv = setinv,
      	getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), then 
## the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {

        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
