##===============================================================================
## This script is aimed at optimising performance relating to matrix inversions,
## in short as follows:
## Once the inverse of a matrix has been calculated, it can be written to cache. 
## Subsequently, the inverse of the same (unchanged) matrix can be read from cache.
##
## Usage:
## See function demoUsage at end.

##===============================================================================
## makeCacheMatrix takes an invertible matrix as input, defines a set of
## functions to facilitate cache access for this matrix and it's inverse,
## and provides these functions as a list.

makeCacheMatrix <- function(x = matrix()) {

    if (det(x) == 0) {
        message("matrix provided is not invertible")
        return(list())
    }
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() {
        x
    }
    setinverse <- function(inverse) {
        i <<- inverse
    }
    getinverse <- function() {
        i 
    }
    list(
        set = set, 
        get = get, 
        setinverse = setinverse, 
        getinverse = getinverse)
}

##===============================================================================
## cacheSolve attempts to read the inverted matrix from cache.
## If the inverted matrix is indeed available in the cache, return it. 
## If there is not yet an inverted matrix in the cache, calculate it 
## (using the solve function), write it to the cache, and return it.

cacheSolve <- function(x, ...) {
    
    i <- x$getinverse()
    if(!is.null(i)) {
        message("inverse read from cache")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    message("inverse calculated using solve")
    x$setinverse(i)
    i
}

##===============================================================================
## Run demoUsage() below on the command line to see how the above functions work.

demoUsage <- function() {
    
    # makeCacheMatrix will show message "matrix provided is not invertible!" 
    # and return an empty list:
    message("demo1:")
    a <- matrix(1:9, 3, 3)
    print(a)
    makeCacheMatrix(a)
    
    # cacheSolve will show message "inverse calculated using solve" once
    # and message "inverse read from cache" twice: 
    message("demo2:")
    p <- matrix(1:4, 2, 2)
    print(p)
    q <- NULL # just in case demoUsage() is run more than once
    q <- makeCacheMatrix(p)
    for (i in 1:3) {
        r <- cacheSolve(q)
    }
    print(r)
}
