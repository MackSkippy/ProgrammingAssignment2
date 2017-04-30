## Functions below cache the inverse of a matrix

## Create matrix object which can cache its own inverse

makeCacheMatrix <- function(x = matrix()) {
##
#initialize m        
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
                }
        get <- function() x
        
        
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        
        #set up to pass defined functions to cahceSolve function
        list(set = set, get = get,
                 setsolve = setsolve,
                 getsolve = getsolve)       
        
  ##      
        
        
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
#check to see if there is already existing matrix
        m <- x$getsolve()
        
#if there is an existing matrix use existing
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
#If there isn't a matrix, use solve to get inverse
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        
#display the matrix
        m
}
