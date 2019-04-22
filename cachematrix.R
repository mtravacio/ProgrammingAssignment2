## Together, the functions makeCacheMatrix and cacheSolve
## compute the inverse of a matrix, if it has already been
## calculated, the result is retreived from the cache.

## makeCacheMatrix creates a special “matrix” object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve computes the inverse of the special “matrix”
## returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        
        ## If it has already been calculated, then the cacheSolve function
        ## should retrieve the inverse from the cache
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        ## If the inverse has not been calculated, the cacheSolve function
        ## computes the inverse of the matrix
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
