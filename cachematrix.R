## Caching the Inverse of a Matrix:
## Create a special object that stores a matrix and caches its inverse.

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
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}

## This function: (1) If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache, else
## (2) compute the inverse of the special "matrix" created by makeCacheMatrix. 

cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    dat <- x$get()
    inv <- solve(dat, ...)
    x$setInv(inv)
    inv
}
