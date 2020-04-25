## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        m <- x$getminverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x[["mat"]]
        m <- solve(data)
        x$setminverse(m)
        m
}
