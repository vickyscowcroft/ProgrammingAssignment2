## This pair of functions can cache the inverse of a matrix. 
## This is to save compuation time instead of repeatedly computing
## the inverse of a matrix

## makeCacheMatrix creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL ## i is the inverted matrix variable, initialising it to NULL
        set <- function(y) { 
                x <<- y    
                i <<- NULL ## initialising the valuee of i to NULL in makeCacheMatrix environment only
        }
        get <- function() x ## create function 'get' in makeCacheMatrix, assign vector x to it
        setinverse <- function(solve) i <<- solve ## Take value 'solve'. Set it to value 'i' in makeCacheMatrix fram
        getinverse <- function() i ## returns value of 'i' from makeCacheMatrix frame
        list(set = set, get = get, ## Lists values of functions in makeCacheMatrix frame
                setinverse = setinverse,
                getinverse = getinverse)
}


## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix
## If the inverse has already been calculated, it retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        i <- x$getinverse() ## retrieve the cached value of i from the 'x' environment
        if(!is.null(i)) {
                ## if the cached value of i is not NULL, retrieve the cached value
                message("getting cached data")
                return(i)
        }
        data <- x$get() ## If this matrix has not been evaluated before, pulls it into 'data'
        i <- solve(data, ...) ## invert the matrix
        x$setinverse(i) # Assign inverted matrix to 'x' environment
        i ## Return the inverted matrix
}
