##makeCacheMatrix: This function creates a special "matrix" object that can 
##cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix"
##returned by makeCacheMatrix above. If the inverse has already been calculated
##(and the matrix has not changed), then the cachesolve should retrieve the
##inverse from the cache.

## makeCacheMatrix return a function list which set or get inverse matrix,
## variable inv and x are still valid outside the function.

makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inversed) inv <<- inversed
	getinverse <- function() inv
	list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Check if the input matrix has already been calcuated, then get the
## corresponding inverse matrix from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if (!is.null(inv)) {
		print("getting cached data")
		return(inv)
	}
	print("solve() must be called")
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}
