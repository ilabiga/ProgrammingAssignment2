## the combination of the following functions allows to calculate and recall the inverse of a matrix.
## the matrix and its inverse are stored in the cache, so that if the matrix doesn't change it is not necessary to recompute its inverse every time the inverse is called.


## makeCacheMatrix is a function containing a list of functions that are useful for storing a matrix and its inverse.
## MakeCacheMatrix uses two variables in the environment: a matrix "y" and its inverse "inverse"
## the "set" function assigns the values of "y" to a matrix "x" (stored in the main function "makecacheMatrix") and restore to null the values of the inverse matrix "inv" (stored in the main function)
## the "get" function returns "x" (which is stored in the main function)
## the "setinv" function assignes the "inverse" values to "inv"
## the "getinv" function returns "inv"

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## cacheSolve checks if the inverse has already been calculated (if yes it recalls its value, if not it caluclates it) and returns the inverse
## "x$getinv()" calls the "getinv" function for a matrix "x"
## if "inv" is not null, "inv" is returned
## oterwhise the values of "x" are called by means of "x$get()" and stored in "data"
## "solve" calculates the inverse of "data", which are thus stored in "inv"
## "inv" is passed to the function "setinv" and then returned

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
