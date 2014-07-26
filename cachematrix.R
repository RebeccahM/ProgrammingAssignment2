## This code contains a pair of functions that can cache the inverse of a matrix,
## in which the calculation of the inverse of the matrix can be computationally
## costly.

## The approach is as follows:
## Set the value of the matrix
## Get the value of the matrix
## Set the value of the inverse
## Get the value of the inverse

## The function makeCacheMatrix create a special matrix

makeCacheMatrix <- function(x = matrix()) {
        
	## initialize value of the inverse matrix
	m <- NULL
        
	## set value using input
	set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
	## get value if it has already been set
	get <- function() x

        setinv <- function(cacheSolve) m <<- cacheSolve
        getinv <- function() m

        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## The cacheSolve function computes the mean of the special matrix 
## returned by the function makeCacheMatrix above. It first checks if the 
## inverse has been calculated. If so, it gets the inverse from the cache.
## If not, it calculates it and puts the value of the mean in the cache
## using the setinv function. 

cacheSolve <- function(x, ...) {
        
	## get value of inverse 
	m <- x$getinv()

	## check to see if inverse has already been calculated
        if(!is.null(m)) {
                message(“Getting cached data…”)

		## if yes, then return value
                return(m)
        }
	
	## if no, then calculate inverse
	invertiblematrix <- x$get()
        m <- solve(invertiblematrix, ...)
        x$setinv(m)
        m
}