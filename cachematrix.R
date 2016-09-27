## makeCacheMatrix and cacheSolve together calculates the inverse of a matrix, and 
## if the matrix inverse has already been calcualted, it will fetch the inverse from 
## the cahce.
## Code written by Nik Buenning, but adapted from
## the Assignment2 Example (cacheing the mean of a vector)

## This function inputs a matrix and creates a list of functions
## so that the matrix inverse can be cached.

makeCacheMatrix <- function(x = matrix()) {
    invs <- NULL
    set <- function(y) {
        x <<- y
        invs <<- NULL
    }
    get <- function() x
    setinvs <- function(dummy) invs <<- dummy
    getinvs <- function() invs
    list(set = set, get = get,
         setinvs = setinvs,
         getinvs = getinvs)
}

# Computes the inverse of the matrix returned by makeCacheMatrix. 
# The function will fetch the inverse from the cache if it has already been calculated.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invs <- x$getinvs() 
    if(!is.null(invs)) {
        message("getting cached data")
        return(invs)
    }
    data <- x$get()
    invs <- solve(data, ...)
    x$setinvs(invs)
    invs
}
