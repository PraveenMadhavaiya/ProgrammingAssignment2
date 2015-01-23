## Put comments here that give an overall description of what your
## The following two functions are part of the Coursera Assignment
## The first function makeCacheMatrix creates an inverse of a square
## matrix and stores it in a Global variable m. 
## The second function cacheSolve looks for this special global variable
## and retrieves the cached value when it is available

## This function makeCacheMatrix sets the matrix in cache

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL ## sets the variable to NULL
		## set function sets the variables in the parent environment
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

        get <- function() x
		##  creates the inverse of the matrix supplied and sets the variable in the cache
        setInverse <- function(solve) m <<- solve 
        getMatrix <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getMatrix = getMatrix)
}

## This function retrieves the inverse of the matrix from the cache if available
##  otherwise it calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
       m <- x$getMatrix()
	   ## checking if inverse is available in the cache
	   ## retrieves the inverse matrix if available
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
		## retrieves the matrix supplied
        data <- x$get()
		## calculates the inverse of the matrix supplied
        m <- solve(data, ...)
		## sets the inverse matrix in cache
        x$setInverse(m)
        m

}