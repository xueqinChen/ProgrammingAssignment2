## There are two functions that are used to create special object that
## stores matrix and cache the input matrix's inverse matrix.
## Especiall, the input matrix should be square invertible matrix.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## x: inverse invertible matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
## if the inverse is cached in makeCacheMatrix, return it; otherwise, calculate it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached matrix data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
