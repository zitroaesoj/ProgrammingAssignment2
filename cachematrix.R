## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse 
## of a matrix rather than compute it repeatedly.

## This function creates a matrix that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse 
	getinverse <- function() inv
	
	list(
		set = set, 
		get = get, 
		setinverse = setinverse, 
		getinverse = getinverse)
}



## This function computes the inverse of the matrix created above.
## If the inverse was already calculated then it will pull the inverse from cache. 
cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	
	if (!is.null(inv)){
		message("pulled from cache")
		return(inv)
	}
	
	mymatrix.data <- x$get()
	inv <- solve(mymatrix.data, ...)
	x$setinverse(inv)
	inv
}
