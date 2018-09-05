## makeCacheMatrix creates list, including four functions and
## original argument, to be assigned and passed to cacheSolve, 
## which either returns cached inverse matrix or calculates, 
## caches and returns new inverse matrix; See below for details


## Sets value of "x" and sets/changes value of "i" (any cached inverse
## matrix calculated by cacheSolve) to NULL; Creates list of four 
## named functions (i.e., $set, $get, $setsolve, $getsolve) to be 
## assigned and passed to cacheSolve; After makeCacheMatrix has 
## been run, $set can be run to change value of "x" and set 
## "i" to NULL

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) i <<- solve
	getsolve <- function() i
	list(set = set, get = get,
			 setsolve = setsolve,
			 getsolve = getsolve)
}


## Retrieves "i" (by calling $getsolve) and returns it with message 
## unless value of "i" is NULL; If value of "i" is NULL, retrieves  
## data (by calling $get), calculates inverse matrix, and caches 
## (by calling $setsolve) and returns result 

cacheSolve <- function(x, ...) {
	i <- x$getsolve()
	if (!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	## Return a matrix that is inverse of 'x'
	data <- x$get()
	i <- solve(data, ...)
	x$setsolve(i)
	i
}
