## Two functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inversem <- NULL
    set <- function(y) {
        x <<- y
        inversem <<- NULL
    }
    get <- function() x
    setinversem <- function(inv) inversem <<- inv
    getinversem <- function() inversem
    list(set = set, get = get,
         setinversem = setinversem,
         getinversem = getinversem)
}


## This function computes the inverse matrix of the "matrix" created by makeCacheMatrix above. 
## If the inverse matrix has already been calculated, 
## then the cacheSolve should retrieve calculated inverse from the cache.

cacheSolve <- function(x, ...) {
    inversem <- x$getinversem()
    if(!is.null(inversem)) {
        message("getting cached inverse matrix")
        return(inversem)
    }
    data <- x$get()
    inversem <- solve(data, ...)
    x$setinversem(inversem)
    ## Return a matrix that is the inverse of 'x'
    inversem
}
