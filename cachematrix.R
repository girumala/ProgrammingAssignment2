## Put comments here that give an overall description of what your
## functions do

## If the same matrix passed:
## first time: Matrix inverse will be calculated
## Second time: Matrix inverse will be from cache


## Write a short comment describing this function
##  list containing functions to
##  1. set the matrix
##  2. get the matrix
##  3. set the inverse
##  4. get the inverse
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


## Write a short comment describing this function
## returns a matrix which is inverse of 'x'
## if it is already cached, reads from cache otherwise calculates the inverse
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