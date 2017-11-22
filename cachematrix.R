## The functions are designed to cache the inverse of a matrix.

## The function makeCacheMatrix creates a special 'matrix' object which can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inv) inv <<- solve
	getinverse <- function() inv
	list(set=set, get = get,
	setinverse = setinverse,
	getinverse = getinverse)
}


## The function cacheSolve computes the inverse of the special matrix created with makeCacheMatrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}
