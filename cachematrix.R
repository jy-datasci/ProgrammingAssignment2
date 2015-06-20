## Put comments here that give an overall description of what your
## functions do
## R Programming assignment 2
## makeCacheMatrix takes a matrix and returns a list of functions to
## get and set the matrix and inverse matrices.
## cacheSolve takes a list returned from makeCacheMatrix and returns the
## inverse matrix.  It is returned from the cached value in the 
## functions that are part of the list have a cached value stored, otherwise
## it is computed and stored.

## Write a short comment describing this function
## store the matrix in the environment and return a list of functions to get and set the inverse
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL # initially the inverse is NULL
        set = function(y) {
            x <<- y # x is stored in the environment for makeCacheMatrix since
                    # it was defined there
            i <<- NULL # i is also stored in the environment of makeCacheMatrix
        }
        # x is found from the enviromnent where the get function is defined
        get <- function() x
        # in setInverse(), i is stored in the environment for makeCacheMatrix
        # since it is defined there
        setInverse <- function(inverse) i <<- inverse
        # i is found from the enviromnent where getInverse is defined
        getInverse <- function(inverse) i
        list(set = set, get = get,
                setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## solve the matrix.  Returns the cached value if it exists, otherwise solves and
## caches the value.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #'x' is a list of funcions returned from makeCacheMatrix
        i <- x$getInverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i) #returns the cached inverse matrix
        }
        data <- x$get() # the input matrix
        i <- solve(data, ...) # the inverse matrix
        # cache the inverse in the environment of the makeCacheMatrix function of x
        x$setInverse(i)
        i
}
