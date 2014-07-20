## makeCacheMatrix creates a special "matrix" that can cache its inverse.
## The inverse of 'x' is available after calling cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
    m <- matrix(, nrow = nrow(x), ncol = ncol(x))
    set <- function(y) {
        x <<- y
        m <<- matrix(, nrow = nrow(x), ncol = ncol(x))
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve calculates and returns a matrix that is the inverse of 'x'
## (assuming that the matrix in this is always invertible).

## If the inverse has already been calculated (and 'x' has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.na(m[1])) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
