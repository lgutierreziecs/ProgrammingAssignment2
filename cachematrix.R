## Considering the script for calculation of means, I developed in ## an analogous way the caching the inverse of the matrix.

## First function. Making a matrix to cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        matInverse <- NULL
        set <- function(y) {
                x <<- y
                matInverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) matInverse <<- inverse
        getInverse <- function() matInverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Second function. Computing the inverse of a matrix

cacheSolve <- function(x, ...) {
        matInverse <- x$getInverse()
        if(!is.null(matInverse)) {
                message("getting cached data")
                return(matInverse)
        }
        matInv <- x$get()
        matInverse <- solve(matInv, ...)
        x$setInverse(matInverse)
        matInverse
}

