## Since matrix inversion may be a costly computation, this 
## makeCacheMatrix function and the accompanying cacheSolve function 
## can be used to save the inverse of a matrix in cache the first time it 
## is computed and then return that inverse each subsequent time it is needed 
## without recomputing it.  Here is a simple example of the use of this
## pair of functions:

## Initialize the list of caching functions assigned to my_m for a 4 x 4 numeric  
## matrix which in this simple case we will generate with 16 random values:
##
##      my_m <- makeCacheMatrix(matrix(rnorm(16), 4, 4))

makeCacheMatrix <- function(x = matrix()) {
        ## 'x' is a square numeric or complex matrix for which the inverse will
        ## be needed repeatedly.  It is assumed to be invertible.
        ##
        ## This function generates a list of four caching functions which
        ## 1) store in and 2) retrieve from cache the matrix passed in the
        ## function call and which 3) store in and 4) retrieve from cache the 
        ## inverse of the passed in matrix.  These functions are called from the
        ## cacheSolve function and therefore make use of a unique environment
        ## to maintain the matrix, it's inverse and the function definitions.
        
        i <- NULL                            ## 1) save the passed matrix in the
        set <- function(y) {                 ## unique environment and initialize  
                x <<- y                      ## the inverse variable 
                i <<- NULL                  
        }

        get <- function() x                  ## 2) return the original matrix

        setinv <- function(inv) i <<- inv    ## 3) initialize i with the passed
                                             ## inverse matrix

        getinv <- function() i               ## 4) return the stored inverse matrix

        list(set = set, get = get,           ## generate the list of 4 functions
             setinv = setinv,                ## in the unique environment
             getinv = getinv)
}


## Now whenever the inverse of the matrix assigned to my_m is needed, 
## instead of invoking solve() directly, use the cacheSolve function
## to  calculate the inverse only once, save it in cache and then efficiently 
## retrieve that inverse matrix from cache on all subsequent calls:
##
##      my_inv <- cacheSolve(my_m)

cacheSolve <- function(x, ...) {
        ## 'x' is the variable to which the makeCacheMatrix function was 
        ## previously assigned.  It identifies a set of functions and an
        ## environment unique to the matrix that was specificed in the 
        ## preceeding makeCacheMatrix function call.
        
        i <- x$getinv()                          ## retrieve the matrix inverse
                                                 ## from cache
        if(!is.null(i))  {                       ## if it is not null
                message("getting cached data")   ## indicate the cached inverse
                return(i)                        ## is being returned
        }
        data <- x$get()                          ## otherwise, get the matrix,
        i <- solve(data, ...)                    ## generate the inverse,  
        x$setinv(i)                              ## store it in cache,
        i                                        ## and return it
}
