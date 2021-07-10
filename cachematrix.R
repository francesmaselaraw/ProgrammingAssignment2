## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        n <- NULL
        set <- function(y){
                x <<- y
                n <<- NULL
                }
        get <- function() x
                setInverse <- function(inverse) n <<- inverse
                getInverse <- function() n
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## A matrix that is the inverse of 'x' will be returned

        ## cacheSolve will compute the inverse of the special "matrix" that will be returned by makeCacheMatrix. If the
## inverse was already calculated, then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## A matrix that is the inverse of 'x' will be returned
        n <- x$getInverse()
        if(!is.null(n)){
                message("Getting cached data")
                return(n)
                }
        mat <- x$get()
        n <- solve(mat,...)
        x$setInverse(n)
        n
}
