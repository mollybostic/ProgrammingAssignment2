## These functions cache the inverse of a matrix.

## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    cachedinverse <- NULL
    
    # sets the matrix value to y
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    # returns the matrix value
    get <- function() { 
        x
    }
    
    # adds the specified inverse value to the cache
    setinverse <- function(i) {
        cachedinverse <<- i
    }
    
    # returns the inverse value from the cache
    getinverse <- function() {
        cachedinverse
    }
    
    # defines the list of operations
    list(set=set, get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}


## Computes the inverse of the special "matrix" (i.e. matrix + cache 
## operations) returned by makeCacheMatrix. If the inverse has already 
## been calculated and the matrix has not changed, then cacheSolve retrieves
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    # attempt to retrieve the value from the cache
    i <- x$getinverse() 
    
    # if a cached value exists, return it
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    # otherwise, calculate the inverse and cache it
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    # then return the inverse value
    i
}
