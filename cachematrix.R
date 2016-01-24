## Takes a matrix x as a parameter
## Creates a list of functions to manipulate the matrix passed (get,set,getinverse & setinverse)
## inv is used to Cache the inverse of x.. the inverse is not calculated here. It is done in the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function (y) {
        x <<- y
        inv <<- NULL
    }
    get <- function () x
    setinverse <- function (solve) inv <<- solve
    getinverse <- function () inv
    
    list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
}


## Takes a cache matrix function list as a parameter
## Calculates and stores the inverse of the matrix defined in x if it does not already exist
## If the inverse does exist (in cache) then it is returned without doing the calculation

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve (data)
    x$setinverse(inv)
    inv
}
