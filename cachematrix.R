## The following functions will cache potentially time-consuming
## computation of matrix inversion

## This function create a special "matrix" object that can 
## cache its inverse

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


## This function computes the inverse of the special "matrix" object
## that can cache its inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cache data")
                return(inv)
        }
        mtrx <- x$get()
        inv <- solve(mtrx, ...)
        x$setinverse(inv)
        inv
}
