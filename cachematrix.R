## Two functions to answer the Week 3 lexical scoping assignment 
## My second version of this, this time with more documentation
## and using x and z rather than x and x; one in the makeCache 
## environment and one in the cacheSolve environment


## This first function creates a special 'bucket' environment to cache the inverse.
## It also creates four functions to assist with this work.

makeCacheMatrix <- function(x = matrix()) {
    ## Define an empty variable to hold the result
	inverse.x <- NULL
	
	## Define four functions
	
	## All these functions are for use outside makeCacheMatrix
	## but refer back here, using <<-, to set and get data stored here
	
	## #1  A function to store new values of x and reset the solution variable
	##     in the event that we have a new x to calculate
    set <- function(y) {
        x <<- y
        inverse.x <<- NULL
    }
	
	## #2 A function to return x 
	##   (from the environment it was defined in, the cache, which is here
	##     in the makeCacheMatrix 'bucket')
    get <- function() x
	
	## #3 A function to store the solution in the cache
    setinverse <- function(solved.x) inverse.x <<- solved.x
	
	## #4 Collect the previously calculated solution from the cache
    getinverse <- function() inverse.x

	
	## Return the four defined functions as a list,
	## where the list names replicate the function names for convenience.
	## This has the effect of making the functions available outside this function.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## This second function computes the inverse of a matrix and stores it in the
## makeCacheMatrix environment.
## If the inverse has already been calculated then 
## cacheSolve retrieves the inverse from the cache
## (NB there is no check that the matrix has not changed)
##      To use it e.g. 
##      firstmatrixwork <-makeCacheMatrix(feed a matrix as data here)
##      cacheSolve(firstmatrixwork)
##      If a new matrix is to be worked on it will need to be via set()

cacheSolve <- function(z, ...) {
    
	## First, don't do the work if it is already done,
	## get it from the cache
	solved <- z$getinverse()
	
	## Check if the work has been done and if so, return it and stop
    if(!is.null(solved)) {
        message("This is the previously cached data")
        return(solved)
    }
	
	## If not, get the target matrix stored in the cache
	## ??? but hang on, can we get() if not already set()?
	## Yes, but only if target already submitted to makeCacheMatrix.
    target <- z$get()
    
	## Use the solve() function to calculate the inverse of z
	solved <- solve(target, ...)
	
	## Store the result in the cache in case we want it again
    z$setinverse(solved)
    
	## Return the result, a matrix that is the inverse of 'z'
	solved
}
