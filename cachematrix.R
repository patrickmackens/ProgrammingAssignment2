## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix: special matrix data structure based on functions
## "set" sets the matrix for which the inverse is supposed to be calculated
## "get" returns the stored matrix
## "setinverse" sets an inverse matrix
## "getinverse" returns the inverse of the matrix if cacheSolve was called after last call of "set"
## otherwise NULL is returned

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(i) inverse <<- i
    getinverse <- function() inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve: Calculates and stores inverse of matrix in makeCacheMatrix data structure
## if inverse of matrix has been calculated before and matrix has not changed, a cached version of the inverse is returned

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached inverse")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
