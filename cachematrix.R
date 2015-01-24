## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function:
## Function [makeCacheMatrix] provides a "pimped" matrix-object, which is able to keep both
## the matrix and its inverse. The return value is a list of four elements 
## pretending an interface of four get and set methods.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv

        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function:
## Function [cacheSolve] is making use of the [makeCacheMatrix]-"object" (i.e. parameter x
## needs to be an object of this type) by returning the inverse of the initial matrix, 
## which both are stored in [makeCacheMatrix].
## In the initial call of [cacheSolve] the inverse will be computed by using function [solve].
## and then made persistent by using "method" [setinverse].

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
    inv<-x$getinverse()
    if(!is.null(inv)){
      message("getting cached data")
      return(inv)
    }
    matrix<-x$get()
    inv<-solve(matrix, ...)
    x$setinverse(inv)
    inv   
        
}
