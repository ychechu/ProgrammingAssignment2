## Caching the Inverse of a Matrix: set of functions

## The first function, makeCacheMatrix creates a special "matrix", which is really 
    ## a list containing a function to
    ## set the value of the matrix
    ## get the value of the matrix
    ## set the inverse of a matrix
    ## get the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}

## The following function calculates the inverse of the special "matrix" created
    ## with the above function. However, it first checks to see if the inverse
    ## has already been calculated. If so, it gets the inverse from the cache 
    ## and skips the computation. Otherwise, it solves the inverse of the matrix
    ## and sets the value of the inverse of the matrix in the cache via the 
    ## SolveInverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
}