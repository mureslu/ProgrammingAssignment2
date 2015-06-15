## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {      ## default argument is ´matrix´
                ## initiatialise the inverted matrix to NULL
        inv <- NULL
                ## define the Set function, for assigning value to the cached matrix
        set <- function(y) {
                ## assign initial values to the matrix and its inverted value
                ## use the righ operator to maintain the values after the calls
                x <<- y
                inv <<- NULL
        }
                ## define the Get function, for assigning value to the cached matrix
        get <- function() x
        setInverted <- function(inverted) inv <<- inverted
        getInverted <- function() inv
        list(set = set, get = get,
             setInverted = setInverted,
             getInverted = getInverted)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverted()
        if(!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverted(inv)
        inv
}

makeCacheMatrix2 <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
                message("Initialising inverted matrix")
        }
        get <- function() x
        getInverted <- function() {
                print(is.null(inv))
                if(!is.null(inv)) {
                        message("Getting cached data")
                } else {
                        inv <<- solve(x)
                        message("Inverting matrix data")
                }
                return(inv)
        }
        
        list(set = set, get = get,
             getInverted = getInverted)
}