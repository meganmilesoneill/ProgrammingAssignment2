## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function is reponsible for taking a standard matrix
## and providing functions for getting/setting the matrix and its inversion

makeCacheMatrix <- function(x = matrix()) {
    # i will store the inverted matrix
    i <- NULL
    
    # setter function to set the matrix
    set <- function(y) {
        x <<- y
        # whenever we set the matrix, 
        # we need to reset the inverted matrix
        # so that it will be recalculated the next time
        # the inverted matrix is accessed
        i <<- NULL
    }
    # getter function to return the matrix
    get <- function() x
    # setter function for the solved/inverted the matrix
    setsolve <- function(solve) i <<- solve
    # getter function to return the solved/inverted matrix
    getsolve <- function() i
    # list exposing the internal functions of the special "matrix" object
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve )
}


## The cacheSolve function is responsible for checking 
## to see if a matrix has already been inverted and cached.
## If it has not yet been inverted, it creates and caches
## the inverted matrix

cacheSolve <- function(x, ...) {
        i <- x$getsolve()
        
        # check to see if the matrix has already been inverted and cached
        if(!is.null(i)){
            message("getting cached data")
            return(i)
        }
        # matrix has not yet been inverted and cached, so we'll do that now
        data <- x$get()
        i <- solve(data, ...)
        x$setsolve(i)
        # return the inverted matrix
        i
}
