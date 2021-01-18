
## Create special matrix for cache control
## R Programming course - Coursera/JHU
## Programming assignment No 2
## by Oscar Zamora

## This assignment it's compose of a pair of functions that cache the inverse of a matrix
## namely makeCacheMatrix (creates a special "matrix" object that can cache its inverse)
## and cacheSolve (function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix).
## If the inverse has already been calculated (and the matrix has not changed), then 
## the cachesolve should retrieve the inverse from the cache.
## For this assignment, we assume that the matrix supplied is always invertible.
##
## The logic of these functions are based on the examples makeVector and cachemean
## given in the instructions for this assignment

## creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    mInverse <- NULL
    set <- function(y) {
        x <<- y
        mInverse <<- NULL ## cada vez que x cambia
    }
    get <- function() x
    setInverse <- function(Inverse) mInverse <<- Inverse
    getInverse <- function() mInverse
    list(set= set, get= get, setInverse= setInverse, getInverse= getInverse)
}


## Computes the inverse of the special "matrix" returned by 
## makeCacheMatrix with solve(x) function in R 
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mInverse <- x$getInverse()
    if(!is.null(mInverse))
    { 
        message("getting cached data") 
        return(mInverse) 
    }
    mInverse <- solve(x$get()) 
    x$setInverse(mInverse)
    mInverse
}
