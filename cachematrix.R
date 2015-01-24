## This source code produces 2 functions to return the inverse of a matrix, without having
## to recalculate the inverse if it has been done already (is in the cache in another environment)
##
##
## To run the inverse of a matrix proceed as follows:
##    1. assign a square matrix to a variable (example 2x2 matrix: x<-matrix(c(1,4,3,7),2,2))
##    2. Get its inverse with : cacheSolve(makecacheMatrix(x))
##

## makeCacheMatrix is a function which returns a list of functions.
## Its puspose is to store a matrix and a cached value of the inverse of the
## matrix. It contains the following functions that are used in the cacheSolve
## function:
## * setMatrix set the value of a matrix
## * getMatrix get the value of a matrix
## * cacheInverse get the cached value (inverse of the matrix)
## * getInverse get the cached value (inverse of the matrix)
makeCacheMatrix <- function(x = matrix()) {
        ## holds the cached value or NULL if nothing is cached
        ## initially nothing is cached so set the cache object to NULL
        cacheMatrix <- NULL
        ## store the matrix newValue in the x object
        setMatrix <- function(newValue) {
                x <<- newValue
                ## since the matrix is assigned a new value, empty the cacheMatrix
                cacheMatrix <<- NULL
        }
        ## returns the stored matrix
        getMatrix <- function() {
                x
        }
        ## cache the inverse of the matrix given as argument
        setInverse <- function(solve) {
                cacheMatrix <<- solve
        }
        ## get the cached inverted matrix value
        getInverse <- function() {
                cacheMatrix
        }
        ## return a list of functions to be used in cacheSolve function. 
        list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve is a function that will invert a matrix. The argument is the makeCacheMatrix outcome 
cacheSolve <- function(x, ...) {
        ## get the cached value into inverseMatrix
        inverseMatrix <- x$getInverse()
        ## if a cached value exists, return it
        if(!is.null(inverseMatrix)) {
                message("getting cached data")
                return(inverseMatrix)
        }
        ## otherwise get the matrix, calculate the inverse and store it in the cache
        matrix <- x$getMatrix()
        inverseMatrix <- solve(matrix)
        x$setInverse(inverseMatrix)
        ## return the inverse Matrix
        return(inverseMatrix)
}