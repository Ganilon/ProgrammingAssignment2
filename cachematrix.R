## File includes functions:
## 	makeCacheMatrix: Creates list of functions to store a matrix and cache 
##			 its inverse.
##	cacheSolve:	 Calculates, caches, and returns the inverse of 
##			 a matrix. 


## This function takes a matrix as argument and returns a list of functions
## 	Functions in list: set, get, setInverse, getInverse

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	
	get <- function() x
	setInverse <- function(inverse) i <<- inverse
	getInverse <- function() i
	
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}



## This function takes a list as argument expecting one created from 
## makeCacheMatrix() and caches and returns the inverse of the list's matrix.
## If the inverse was already calc'd it will return the cached result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getInverse()
	if(!is.null(i)) {
		message("Getting cached data...")
		return(i)
	}
	data <- x$get()
	i <- solve(data)
	x$setInverse(i)
	i
}
