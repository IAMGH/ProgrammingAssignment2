## Two functions to answer the Week 3 lexical scoping assignment 

## This first function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse.x <- NULL
    set <- function(y) {
        x <<- y
        inverse.x <<- NULL
    }
    get <- function() x
    setinverse <- function(solve.x) inverse.x <<- solve.x
    getinverse <- function() inverse.x
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## This second function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache
##      To use it e.g. 
##      testhiscache <-makeCacheMatrix(feed a matrix as data here)
##      cacheSolve(testhiscache)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	solved <- x$getinverse()
    if(!is.null(solved)) {
        message("getting cached data")
        return(solved)
    }
    target <- x$get()
    solved <- solve(target, ...)
    x$setinverse(solved)
    solved
}
