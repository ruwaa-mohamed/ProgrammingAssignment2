## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## A function very similar to the example makeVector function that creates a 
## special matrix (very close to OOP) with 4 special functions to get and set 
## the matrix and get and set its inverse. This function itself doesn't 
## calculate the inverse of the matrix but is saved into it when the other 
## function is called.
makeCacheMatrix <- function(x = matrix()) {
    x.inv <- NULL
    set <- function(y) {
        x <<- y
        x.inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) x.inv <<- inverse
    getinverse <- function() x.inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## a function very similar to the example cachemean that calculates the inverse
## of the special matrix if not calculated before by the solve function then 
## saves it in the special matrix object. If it was calculated before, it
## retrieves the cached inverse.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    x.inv <- x$getinverse()
    if(!is.null(x.inv)) {
        message("getting cached data")
        return(x.inv)
    }
    data <- x$get()
    x.inv <- solve(data, ...)
    x$setinverse(x.inv)
    x.inv
}
