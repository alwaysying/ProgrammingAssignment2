## The following two functions will compute and cache the inverse of a matrix.

## The makeCacheMatrix creates a special matrix object, which can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

	inv <- NULL
	set <- function(y){
		
		x <<- y
		inv <<- NULL
	}

	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function () inv
	list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## The cacheSolve function computes the inverse of the matrix returned by makeCacheMatrix function. 
## The function will first check if the inverse of the matrix has been computed or not. If yes, it 
## will retrieves the inverse directly.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if(!is.null(inv)) {
		
		message("getting cached data")
	}
	data <- x$get()
	inv <- solve(data,...)
	x$setinv(inv)
	inv
}
