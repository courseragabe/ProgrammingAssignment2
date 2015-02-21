## This file provides functions to cache matrix inverses.
## Two functions are provided:
## makeCacheMatrix: stores a matrix and provides inner
##                  functions to cache and retrieve the matrix's inverse.
## cacheSolve: receives a cached matrix as argument and computes
##             the matrix's inverse. If available, the cached inverse
##             is returned. If not, the "solve" method is invoked.

## Creates a Cache Matrix to store and retrieve the matrix's inverse.
## 
## The following inner functions are available:
## get: gets the matrix to be cached
## set: sets the matrix to be cached
## getSolve: gets the matrix inverse. The cached inverse is
##           returned if available.
## setSolve: caches the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y)
    {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) inv <<- solve
    getSolve <- function() inv
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## Returns the Cache Matrix's inverse. The cached inverse is
## returned if it was alerady computed and stored in the cache.
## Otherwise, the default "solve" method will be used.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getSolve()
    if(!is.null(inv))
    {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setSolve(inv)
    inv
}

## Unit testing
## (uncomment bellow to test)

# mat1 = matrix(runif(25, -5, 5), 5)
# cache1 = makeCacheMatrix(mat1)
# cacheSolve(cache1)
# cacheSolve(cache1)
# cacheSolve(cache1)

# mat2 = matrix(runif(100, -10, 10), 10)
# cache2 = makeCacheMatrix(mat2)
# cacheSolve(cache2)
# cacheSolve(cache2)
# cacheSolve(cache2)

