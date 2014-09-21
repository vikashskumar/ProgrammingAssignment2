##########################################################
## This set of API's help a user create Matrix objects and
## cache its inverse so that inverse only is computed when
## absolutely necessary.
##########################################################


##########################################################
# Accepts a matrix, assumption is the matrix is invertible
# Stores the matrix and provides methods to set / access
# the matrix / its inverse
##########################################################

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inv1) inv <<- inv1
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


##########################################################
# Accepts a list created by makeCacheMatrix and uses
# its functions to find inverse of matrix only if
# it is not already computed. Returns the cached inverse
# or the newly computed inverse as applicable.
##########################################################

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
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

