## The first function serves to create an object by which one can identify
## a matrix and cache its inverse. The second can discern if the matrix
## input has changed and return the appropriate inverse either from cache
## or solve.

## This function creates an object that can cache an inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverseSolve) inv <<- inverseSolve
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function outputs the inverse matrix, either from cache if the 
## matrix remains the same or from solve.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
