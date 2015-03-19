## Below are two functions that are used to create a special
## object that stores a matrix and cache's its inverse.

## This function creates a special matrix object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already
## been calculated, then the cachesolve retrieves the inverse
## from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
