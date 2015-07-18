## This file contains fucntions for a special type of matrix.
## This matrix caches the calculated inverse of the matrix
## Supported functions:
## - makeCacheMatrix:   create the matrix
## - cacheSolve:        get the inverse of the matrix


## makeCacheMatrix creates a special 'matrix', which supports the following functions:
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse
## - get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Calculate the inverse of a special 'matrix' created by makeCacheMatrix
## If the inverse is calculated and the matrix has not changed,
## the cached inverse is returned. Otherwise, the inverse is calculated.
## The function only supports square matrixes. If the input matrix is
## not square it will output a message and return NA
cacheSolve <- function(x, ...) {
        
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()

        if (ncol(data) != nrow(data)) {
                message("cacheSolve only supports square matrices")
                return(NA)
        }
        
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
