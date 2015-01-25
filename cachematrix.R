## John Williams
## San Bernardino 2015
## R Programming - Assignment 2 - Coursera
## ---------------------------------------


## This file defines two functions:
## makeCacheMatrix(x=matrix())
## cacheSolve(x, ...)
##
## These functions are designed to avoid calculating
## the inverse of a square matrix repeatedly by caching
## the valuse of the inverse matrix, so that on subsequent
## computations the inverse can be retrieved from the 
## rather than recomputed.


## makeCacheMatrix(x)
##
## x is a square matrix, assumed to be invertible. No error
## checking is done to make sure the input to makeCacheMatrix
## is valid.
##
## Given an invertible square matrix x, makeCacheMatrix
## returns a list object.  The object returned my
## this function can be passed to cacheSolve to either
## compute the inverse, on the first call, or retrieve
## the inverse on subsequent calls.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve(x, ...)
## 
## x is a list object created by makeCacheMatrix.
## No input checking is done on the input object
## passed to the function.
## 
## Additional arguments passed to cacheSolve will
## will be passed to the solve function, but as it
## stands the function is mean to be called with
## with only the "matrix" object x, in which case
## it returns the inverse of x, computed with solve.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
