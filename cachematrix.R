## As matrix inversion is a computation heavy task these two functions provide
## a way to cache the result:
## To use this first generate a "cache matrix":
## cache.matrix <- makeCacheMatrix(my.matrix)
## This cache matrix can then be used to compete the inverse of the matrix:
## cacheSolve(cache.matrix)

## Create a special "cache matrix" from a matrix that can cache the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Calculate the inverse of a "cache matrix" using the solve function:
## If the inverse was already calculated it just takes the result from cache 

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
