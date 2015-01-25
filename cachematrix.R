## Function - Caching the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.
## set - for setting the value of the matrix
## get - for getting the value of the matrix
## setinverse -for setting the value of the inverse of the matrix
## getinverse -for getting the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    imat <- NULL
    set <- function(y) {
        x <<- y
        imat <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) imat <<- inverse
    getinverse <- function() imat
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by  makeCacheMatrix.

cacheSolve <- function(x, ...) {
    imat <- x$getinverse() ## data from Cache
    if(!is.null(imat)) {
        print("Fetching Data from cache")
        return(imat)
    }
	print("Calculating Inverse of Matrix")
    matrx <- x$get()
    imat <- solve(matrx, ...)
    x$setinverse(imat) ## set data in cache
    return(imat)
    ## Return a matrix that is the inverse of 'x'
}
