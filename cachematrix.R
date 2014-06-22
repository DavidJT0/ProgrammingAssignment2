## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
## rather than computing it repeatedly. The following functions cache and calculate the inverse of a matrix.
##------------------------------------------------------------------------------------------------------------------


## This function creates a special "matrix" object. This object is a list of 4 functions: get/set/setinv/getinv. 
## The functions can be used to store and modify the values of the matrix and its inverse.

	##[1] the variable "inv" contains the cached inverse matrix. it is initialised as NULL
	##[2] "set" function gets matrix values and reinitialise the value of cached inverse matrix as NULL
	##[3] "get" function returns the matrix stored in the variable "x"
	##[4] "setinv" gets the value of the inverse matrix and store it as cached inverse matrix in the variable "inv" 
	##[5] "getinv" function returns the inverse matrix cached  	
	##[6] the list containing the four functions above is created

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL					##[1]
	set <- function(y) {				##[2]
      		x <<- y
            	inv <<- NULL
        }
	get <- function() {x}				##[3]
	setinv <- function(inverse) {inv <<- inverse}	##[4]
	getinv <- function() {inv}			##[5]
	list(set = set, 				##[6]
		get = get,
      		setinv = setinv,
      		getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), then 
## the cachesolve retrieves the inverse from the cache.

        ##[1] gets the inverse matrix value from the special "matrix" returned by makeCacheMatrix calling the function "getinv" 
	##[2] if the variable is not NULL (there is a value stored) then return the value stored and exit the function cacheSolve
	##[3] if no value is retrieved then get the matrix value and copy it in the variable Data
	##[4] calculates the inverse matrix using the function "solve"
	##[5] sets the inverse matrix value to the special "matrix" returned by makeCacheMatrix calling the function "setinv"
	##[6] returns the inverse matrix value just calculated	

cacheSolve <- function(x, ...) {
	

	inv <- x$getinv()				##[1]
	if(!is.null(inv)) {return(inv)}			##[2]
        data <- x$get()					##[3]
        inv <- solve(data, ...)				##[4]
        x$setinv(inv)					##[5]
        inv						##[6]
}

