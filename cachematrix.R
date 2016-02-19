## Programming Assignment 2: Lexical Scoping
##
## These functions allow you to create a "special" matrix type that can cache
## the inverse of the matrix.  

## This function creates a "special" matrix type that can cache it's inverse.
## Note: It is assumed that the matrix is square and invertible

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(inversematrix) i <<- inversematrix
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of a "special" matrix defined by the
## function makeCacheMatrix. It returns the inverse from cache if it is available,
## otherwise it computes the inverse and caches the inverse it just computed

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        ##message("returning cached data")
        return(i)
    }
    data <- x$get()
    i <-  solve(data, ...)
    x$setinverse(i)
    i
}
