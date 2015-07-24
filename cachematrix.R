## Flavius Popan 7/24/2015 v1
## R Library for caching the inverse of a matrix

## Takes a matrix and returns a list of functions for
## getting and setting the value as well as the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse,
         getinverse = getinverse)
}

## Solves the matrix and returns the result after first calculation
## or returns the cached value if the original matrix hasn't been changed

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
