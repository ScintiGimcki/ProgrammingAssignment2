## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	## initialize the cache
	inv <- NULL
	## the function to initialize
	set <- function(y){
		y <<- x
		inv <<- NULL
	}
	set(y)
	## the function returning y
	get <- function() y
	## the function assigning inverse to inv
	setinv <- function(inverse) inv <<- inverse
	## the function getting inv
	getinv <- function() inv
	## return such a matrixcache
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	## get the inverse of the matrix x
	inv <- x$getinv()
	## if the inverse has never been calculated
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	## get the initialized data
	data <- x$get()
	## compute the inverse of x
	inv <- solve(data, ...)
	x$setinv(inv)
      ## Return a matrix that is the inverse of 'x'
	inv
}