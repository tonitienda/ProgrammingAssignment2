## The following functions allow to cache the result of the solve(m) function
## so once calculated it does not need to be computed again.

## This functions returns a structure (list) to store
## already computed values of the solve(m) function
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Caches the inverse matrix
# The first time the method is called the inverse
# is calculated and saved. 
# The subesequent calls will only return the value.
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

