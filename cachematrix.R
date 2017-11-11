## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than computing it repeatedly 
## With the functions below you can create a special matrix and compute the inverse
## of the matrix. If the matrix hasn't changed, the cached version will be returned.

## Create a "matrix" with the possibility to cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    ## Set matrix function
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## Get matrix function
    get <- function() x
    
    ## Set inverse matrix function
    setinverse <- function(inverse) m <<- inverse

    ## Get inverse matrix function    
    getinverse <- function() m
    
    ## Define the set/get/setinverse/getinverse functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Compute and return the inverse of matrix x and cache the result.
cacheSolve <- function(x, ...) {
    ## Retrieve inverse
    m <- x$getinverse()
    
    ## Do we have a cached version?
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## Get the data of the matrix
    data <- x$get()
    
    ## Compute the inverse of the matrix
    m <- solve(data, ...)
    
    ## Cache the inverse of the matrix
    x$setinverse(m)
    
    ## Return the inverse of the matrix
    m
}
