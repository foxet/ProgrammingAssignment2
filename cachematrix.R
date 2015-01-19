## Matrix inversion is usually a costly computation and their my be some benefit to caching the 
## inverse of a matrix rather than compute it repeatedly.My assignment is to write a pair of 
## functions that cache the inverse of a matrix.
## For this assignment, assume that the matrix supplied is always invertible
## for example:
# a<-matrix(c(2,2,3,1,-1,0,-1,-2,1),nrow=3)
# x<-makeCacheMatrix(a)
# cacheSolve(x) # first call
# cacheSolve(x) # second call, Message ""getting cached data" should return

## This function creates a special 'matrix' object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function compute the inverse of the special 'matrix' returned by makeCacheMatrix above.
## If the inverse has already been calculated(and the matrix has not changed),then the cachesolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
