## Caching the inverse of a matrix:
## Matrix inversion has a high computational cost.
## We can benefit cache storage operations in the inverted matrix ## rather than calculate over and over again.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) inv <<- inverse
        getInv <- function() inv
        list(set = set,
             get = get,
             setInv = setInv,
             getInv = getInv)
}


## This function calculates the inverse of the "matrix" special that was created by
## MakeCacheMatrix. If the reverse was already obtained (and the
## Matrix has not changed) then retrieves the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInv(inv)
        inv
}
