## Since matrix inversion may be a costly computation, this 
## makeCacheMatrix function and the accompanying cacheSolve function 
## can be used to save the inverse of a matrix in cache the first time it 
## is computed and then return that inverse each subsequent time it is needed 
## without recomputing it.  Here is a simple example of the use of this
## pair of functions:

## Initialize the caching functions for a 4 x 4 numeric matrix identified as 
## my_m which in this simple case we will define with 16 random values:
##
##      my_m <- makeCacheMatrix(matrix(rnorm(16), 4, 4))

## Now whenever the inverse of the matrix assigned to my_m is needed, 
## instead of invoking solve() directly, use the cacheSolve function
## to  calculate the inverse only once and then efficiently 
## retrieve that inverse matrix from cache on all subsequent calls:
##
##      my_inv <- cacheSolve(my_m)

makeCacheMatrix <- function(x = matrix()) {
        ## 'x' is a square numeric or complex matrix for which 
        ## the inverse will be needed repeatedly.
        
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get, 
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        i <- x$getinv()
        if(!is.null(i))  {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
