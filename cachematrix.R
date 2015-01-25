## Description of what the functions do:
## The functions makeCacheMatrix and cacheSolve. The first one function creates 
## a "matrix" object with opening possibility to cache its inverse. It is a list of functions:
## set the value of the matrix, get it, set the value of the solve, get it.
## The second one "computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the cacheSolve 
## should retrieve the inverse from the cache."

## Describing this function:
## "This function creates a special "matrix" object that can cache its inverse".

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(z) m <<- z
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## Describing this function:
## "This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix", and retrieve the inverse from the cache
## if inverse has already been calculated and matrix the same.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m    
}