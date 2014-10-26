## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix creates a list containing a function to
# set the matrix,
# get the matrix,
# set the inverse matrix, 
# get the inverse matrix. 

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function( y ){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function( i ) inv <<- i
	getinv <- function() inv
	list( set = set, get = get,
	     setinv = setinv,
	     getinv = getinv)
}

# cachSolve function calculates the inverse
# matrix but if it is found already calculated,
# it gets inverse matrix from the cache.
# When it calculated the inverse matrix,
# it stores the inverse matrix to the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if ( !is.null(inv) ){
		message( "getting cached data")
		return (inv)
	}
	data <- x$get()
	inv <- solve( data, ... )
	x$setinv( inv )
	inv
}
