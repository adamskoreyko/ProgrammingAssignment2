# Functions that have the ability to cache the inverse of a matrix

# Create a special "matrix", which is a list containing
# a function to
#   - set the value of the matrix
#   - get the value of the matrix
#   - set the value of the inverse matrix
#   - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    a <- NULL
    set <- function(y) {
        x <<- y
        a <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) a <<- inv
    getinverse <- function() a
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


# Calculate the inverse of the special "matrix" created with the 
# function, reusing cached result if it is available for computation.

cacheSolve <- function(x, ...) {
    a <- x$getinverse()
    if(!is.null(a)) {
        message("getting cached data")
        return(a)
    }
    m <- x$get()
    a <- solve(m, ...)
    x$setinverse(a)
    a
}
