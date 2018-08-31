## makeCacheMatrix is a function that takes a matrix x, 
##and adds a cache for the inverse of the matrix (if possible).

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve wil return the inverse of matrix x, by first checking the cache.
## If the cache is empty, it will calculate and return the inverse.

cacheSolve <- function (x = matrix()) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setinverse(inverse)
        inverse
}