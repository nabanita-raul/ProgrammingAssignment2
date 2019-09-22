makeCacheMatrix <- function(x = matrix()) {
    inverse_x <- NULL
    
    set <- function(y) {
        x <<- y
        inverse_x <<- NULL
    }
    
    get <- function() x
    setInverse <- function(inverse) inverse_x <<- inverse
    getInverse <- function() inverse_x
    list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Takes as an input a variable of type makeCacheMatrix.
## Calls getInverse() on the called object
## If getInverse is null (i.e. it has not been computed yet), 
##   1. Computes the inverse using solve
##   2. Sets the inverse_x for future cached returns
##   3. Returns the inverse
## If getInverse is not null, returns its output of getInverse. This is the cached value previously computed
##
## Calls to cacheSolve can use all parameters available to solve. For this we calculate the identity matrix of
## the input matrix first. We use this as the 2nd parameter to solve 
## and pass all remaining parameters of cacheSolve to solve
## 
## Example usage:
## x <- matrix(rnorm(16), 4, 4)
## x0 <- makeCacheMatrix(x)
## inverse_x <- cacheSolve(x0)
## Validate: x %*% inverse_x
## Call again to make sure cache is used 
## inverse_x <- cacheSolve(x0)
## Validate: x %*% inverse_x

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if (! is.null(inverse)) {
        message("Getting cached inverse matrix")
        return(inverse)
    }
    data <- x$get()
    identity_matrix <- diag(dim(data)[1])
    inverse <- solve(data, identity_matrix, ...)
    x$setInverse(inverse)
    inverse
}
