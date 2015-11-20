## Put comments here that give an overall description of what your
## functions do

## creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(X = matrix()) {
    Inv <- NULL
    set <- function(y) {
        X <<- y
        Inv <<- NULL
    }
    get <- function() X
    setInverse <- function (y){
        Inv<<- y
    }
    getInverse <- function() Inv
    list(set = set, get = get, setInverse=setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## Attempts to get the inverse from the matrix-like object
    Inv <- x$getInverse()
    
    ## If such inverse exists, return it
    if(!is.null(Inv)) {
        message("getting cached matrix inverse")
        return(Inv)
    }
    
    ## Otherwise, get the matrix and calculate the inverse
    X <- x$get()
    Inv <- solve(X)
    
    ## Store the inverse for future uses
    x$setInverse(Inv)
    
    ## return the inverse
    Inv
}
