## Put comments here that give an overall description of what your
## functions do
# These functions are used to cache the inverse matrix, if the matrix is cached 
# already, we will use it directly from cache instead of recompute. 
# If the inverse matrix is not calculated before, the inverse matrix is computed

## Write a short comment describing this function
# save inverse matrix to cache
# build-in functions include: set(), get(), setinverse(), getinverse()

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
# get inverse matrix from cache or calculate inverse matrix
# if the inverse matrix is saved in cache already, get the inverse matrix
# if the inverse matrix is not saved, compute inverse matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached reverse matrix")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setinverse(inv)
    inv
}
