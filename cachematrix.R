## Put comments here that give an overall description of what your
## functions do
## R Programming assignment 2

## Write a short comment describing this function
## store the matrix in the environment and return a list of functions to get and set the inverse
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set = function(y) {
            x <<- y
            i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function(inverse) i
        list(set = set, get = get,
                setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## solve the matrix.  Returns the cached value if it exists, otherwise solves and
## caches the value.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
