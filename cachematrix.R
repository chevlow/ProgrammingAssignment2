## Caches the inverse of a matrix

## Creates a "special" matrix object that can cache its inverse
## @x: a square invertible matrix
## return: a list containing functions to
##  1. set the matrix
##  2. get the matrix
##  3. set the inverse
##  4. get the inverse
## this list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function () inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## Computes the inverse of the special "matrix" returned by makeCacheMatrix()
## If the inverse has already been calculated, then the cacheSolve() should 
## retrieve the inverse from the cache

## @x: output of makeCacheMatrix()
## return: inverse of the original matrix input to makeCacheMatrix()

cacheSolve <- function(x, ...) {
    inv = x$getinv()
       
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
        
    mat.data <- x$get()
    inv = solve(mat.data, ...)
    
    x$setinv(inv)
    
    return(inv)
}
