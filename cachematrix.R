## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## ------------------------------------------------------------------------- ##

## makeCacheMatrix defines a function that allows the creation of an persistent matrix object ...
## and the encapsulated functions to manipulate it
makeCacheMatrix <- function(x = matrix()) {
        ## default argument is a matrix
        ## initiatialise the inverted matrix to NULL
        inv <- NULL
        
        ## define the Set function, for assigning value to the matrix, to be cached
        set <- function(y) {
                ## assign initial values to the matrix and its inverted value
                ## use the appropriate operator to maintain the values of x and inv after the calls to Set
                x <<- y
                inv <<- NULL
        }
        
        ## define the Get function, for returning the value of the stored matrix
        get <- function() x
        
        
        ## define the Set and Get functions for the inverted matrix
        setInverted <- function(inverted) inv <<- inverted
        getInverted <- function() inv
        
        ## returns the list of functions available (only) to the special cached-matrix object
        list(set = set, get = get,
             setInverted = setInverted,
             getInverted = getInverted)
}

## cacheSolve defines a function for a caching mechanism for inverting matrix objects
## It should accept matrix-objects created with makeCacheMatrix
## It should return a persistent (cached) inverted matrix and calculate the inverted value only once
cacheSolve <- function(x, ...) {
        ##  read and check if the matrix was already inverted; x must be an object created with makeCacheMatrix
        ## return the inverted matrix without any calculation if success
        inv <- x$getInverted()
        if(!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        }
        
        ## if the matrix was not inverted, then invert it and store the result for future calls
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverted(inv)
        
        ## return the inverted matrix, ´fresly´computed
        inv
}

## a scenario of the usage for the cache above would be:
## x<-makeCacheMatrix()
## x$set(rbind(c(1, -1/4), c(-1/4, 1)))
## cacheSolve(x)   ## -> calculates and returns the inverted matrix
## cacheSolve(x)   ## -> returns the cached inverted matrix
## ------------------------------------------------------------------------- ##


## The mechanism above is not great, and the description of the encapsulation through an example is pretty sad
## a weakness is for instance that x$setInverted() can be called directly, not just from cacheSolve()
## meaning one can set any value as the inverted matrix, without being the computed inverted matrix
## I would recommend to remove the permission to change the inverted value, but use another principle:
## Compute the inverted value the 1st time the value is requested, unless if cached already
## i.e. do not even create a function to set the value for inverted 
makeCacheMatrix2 <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        getInverted <- function() {
                if(!is.null(inv)) {
                        message("Getting cached data")
                } else {
                        inv <<- solve(x)
                        ## the only difference from the previous version is that we compute the inverted matrix
                        ## when it is first requested
                        message("Inverting matrix data for first request")
                }
                return(inv)
        }
        
        list(set = set, get = get, getInverted = getInverted) ## and limit the functions that can access the matrix
}

## This version does not require the cacheSolve extra function, as the cache is built-in
## a scenario of the usage for the cache above would be:
## x<-makeCacheMatrix2()
## x$set(rbind(c(1, -1/4), c(-1/4, 1)))
## x$getInverted()   ## -> calculates and returns the inverted matrix
## x$getInverted()   ## -> returns the cached inverted matrix