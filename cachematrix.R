## makeCacheMatrix creates objects that are used with cacheSolve to calculate
## the matrix inverse or return a cached value if the inverse has already been
## calculated

## makeCacheMatrix stores a matrix and returns a list of functions that operate
## on the stored matrix
## setinverse and getinverse are used by cacheSolve to get and set the cached
## value

makeCacheMatrix <- function(x = matrix()) {
    icache <- NULL
    set <- function(y) {
        x <<- y
        icache <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) icache <<- inverse
    getinverse <- function() icache
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve takes as an argument an object created by makeCacheMatrix. It
## will check if the object has a cached value for the inverse, and in that
## case return the cached value. If no cached value is available, it will
## calculate the inverse and cache the value for future use,
## then return the calculated inverse.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse();
    # Check if we have a cached value to return
    if(!is.null(inverse)) {
        message("Getting cached data")
        return(inverse)
    }
    # No cached value was available, get the matrix data to calculate
    m <- x$get()
    # Calculate the inverse (passing any additional arguments to solve)
    inverse <- solve(m, ...)
    # Store the calculated value
    x$setinverse(inverse)
    inverse
}

## Just for testing, not part of the assignment
## Test the functions above 

testCacheMatrix <- function(m) {
    cm <- makeCacheMatrix(m)
    i <- cacheSolve(cm)
    ## Matrix multiplied by inverse should give identity matrix
    r <- m %*% i
    ## Use all.equal to compare since the result might not be exact (i.e. 1e-16 instead of 0)
    if(!isTRUE(all.equal(r, diag(nrow(m))))) {
        print("Test failed, cacheSolve result incorrect.")
        r
    } else {
        print("Test success, cacheSolve result correct.")
    }

    ## Test again for cached result
    i <- cacheSolve(cm)
    r <- m %*% i
    if(!isTRUE(all.equal(r, diag(nrow(m))))) {
        print("Test failed, cacheSolve cached result incorrect.")
        r
    } else {
        print("Test success, cacheSolve cached result correct.")
    }    
}



