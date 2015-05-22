## Below are two functions that are used to create a 
## special object that stores a numeric matrix and 
## cache's its inverse.

## The first function, makeCacheMatrix creates a special 
## "vector", which is really a list containing a function to 
##   1. set the matrix
##   2. get the matrix
##   3. set the inverse of the matrix
##   4. get the inverse of the matrix
##
## Note that the MASS package must be loaded for ginv().

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(ginv) m <<- ginv
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The following function calculates the inverse of the 
## matrix using the special "vector" created with
## the above function.  However, it first checks 
## to see if the inverse has already been calculated. If 
## so, it gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the inverse of 
## the matrix and sets the matrix in the cache via the 
## setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- ginv(data, ...)
        x$setinverse(m)
        m
}
