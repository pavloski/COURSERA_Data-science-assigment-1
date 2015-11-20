## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computed it
## repeatedly. These funcitions use lexical scoping to create an matrix-like
## object and use it to store the matrix inverse, once computed.
## The object also provides internal functions to get and set matrix values
## and its inverse


## creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(X = matrix()) {
    ## initializes the matrix object
    Inv <- NULL
    
    ## function to set the value of the matrix
    set <- function(y) {
        X <<- y
        Inv <<- NULL
    }
    
    ## function to get the matrix
    get <- function() X
    
    ## function to set the value of the inverse of the matrix
    setInverse <- function (y){
        Inv<<- y
    }
    
    ## function to obtain the inverse of the matrix
    getInverse <- function() Inv
    
    list(set = set, get = get, setInverse=setInverse,
         getInverse = getInverse)
}


## Return a matrix that is the inverse of 'x' 
cacheSolve <- function(x, ...) {
    
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
