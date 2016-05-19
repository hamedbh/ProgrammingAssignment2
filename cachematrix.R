## Using the superassignment operator "<<-" to cache the inverse of a matrix,
## which can save computational power.

## Function makeCacheMatrix creates a special object matrix that can cache its
## inverse

makeCacheMatrix <- function(x = matrix()) {
    inv = NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function () x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve will calculate the inverse of the matrix created above, unless
## the inverse has already been calculated.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
## Return a matrix that is the inverse of 'x'
    inv
}