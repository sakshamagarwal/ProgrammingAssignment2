## This is the 2nd programming assignment of R-programming
## These functions can be used to cache the inverse of a matrix

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
## creating function set
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
## creating function get
	get <- function() x
## creating function setinverse
	setinverse <- function(inverse) inv <<- inverse
## creating function getinverse
	getinverse <- function() inv
## returning the list of functions set, get, setinverse, getinverse
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse 
## from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets 
## the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
## if inv has the inverse it is returned
	if(!is.null(inv)) {
		message("getting cached data.")
		return(inv)
	}
## otherwise we calculate the inverse and then return inv
	data <- x$get()
	inv <- solve(data)
	x$setinverse(inv)
	inv
}
