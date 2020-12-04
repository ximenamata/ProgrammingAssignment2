## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() {x}  ##this is the function to get matrix x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() {inv} ##function to get the inverse of matrix x
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
} 

cacheSolve <- function(x, ...) {  ##function to get cache data
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("getting cache data")
        return(inv) ##returns inverse
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv  ##returns a matrix that is inverse to x
}