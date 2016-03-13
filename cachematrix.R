## Put comments here that give an overall description of what your
## functions do

## Returns an object that represents a matrix with a 'cachable' inverse operation.
## Usage: > mInv <- makeCacheMatrix(someMatrix) 

makeCacheMatrix <- function(x = matrix()) {
    ## create variable for cached 'inverse' of the cached 'data' matrix 'x'
    inv <- NULL
    set <- function(y){
        x <<- y
        ## NULL the cached inverse as the data has changed
        inv <<- NULL
    }
    get <- function() {
        x
    }
    setinverse <- function(newinverse){
        inv <<- newinverse
    }
    getinverse <- function(){
        inv
    }
    ## return an object (list) whose 4 elements are the functions defined above
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Takes an object returned from the 'makeCacheMatrix()' function and calculates
## the inverse of the matrix contained in that object, if it has not been cached.
## Otherwise it returns the cached inverse, which is also contained in that object.

cacheSolve <- function(x, ...) {
    ## retrieve the cached inverse contents
    inverse <- x$getinverse()
    ## is it not null ie has it been calculated before
    if(!is.null(inverse)){
        ## yes, so just return that
        message("getting cached inverse")
        return(inverse)
    }
    ## if not then we need to calculate it, so get the data contents
    data <- x$get()
    ## calculate the inverse
    inverse <- solve(data)
    ## update the cached inverse
    x$setinverse(inverse)
    ## and return the new inverse
    return(inverse)
}
