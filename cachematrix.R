
# The 2 functions here provide a mechanism to cache the inverse of a matrix. Calculating
# the inverse of a matrix can be a computational intensive operation. The use of cache
# helps to speed up the efficiency of the calculation.

## Creates a list containing the matrix and the functions to get/set the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        setminverse <- function(minverse) m <<- minverse
        getminverse <- function() m
        list(set = set, mat  =  x,
             setminverse = setminverse,
             getminverse = getminverse)
}


## Takes in the results from makeCacheMatrix() and determine whether cached data is available.
## If cached data is available, it takes cached data. If not, it calculate and set as cached
cacheSolve <- function(x, ...) {
        m <- x$getminverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x[["mat"]]
        m <- solve(data, ...)
        x$setminverse(m)
        m
}
