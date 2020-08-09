## Put comments here that give an overall description of what your
## functions do

## This assignment is to write a pair of functions that cache the inverse of a matrix.


## Write a short comment describing this function
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

##The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##1. set the value of the matrix
##2. get the value of the matrix
##3. set the value of the inverse
##4. get the value of the inverse



makeCacheMatrix <- function(x = matrix()) {
            invvar <- NULL
            set <- function(y) {
                    x <<- y
                    invvar <<- NULL
            }
            get <- function() x
            setInverse <- function(inverse) invvar <<- inverse
            getInverse <- function() invvar
            list(set = set, 
                 get = get,
                 setInverse = setInverse,
                 getInverse = getInverse)
}


## Write a short comment describing this function 
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invvar <- x$getInverse()
        if(!is.null(invvar)) {
                message("getting cached data")
                return(invvar)
        }
        matrixdat <- x$get()
        invvar <- solve(matrixdat, ...)
        x$setInverse(invvar)
        invvar
  
}
 

