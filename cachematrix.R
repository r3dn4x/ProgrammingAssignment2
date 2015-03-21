## These pair of functions cache the inverse of a matrix, a costly computation.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { ## Initializes a function that
                                            ##   takes a matrix as its argument.
        i <- NULL                           ## Sets a local variable, i (inverse
                                            ##   of the matrix), to NULL.
        set <- function(y) {                ## Caches the value of x to the new
                x <<- y                     ##   passed matrix (y). Resets the 
                i <<- NULL                  ##   inverse, i, back to NULL.
        }
        get <- function() x                 ## This function takes no argument
                                            ##   and simply "gets" (returns) x.
        setinverse <- function(solve)       ## Finds the inverse of the matrix
                 i <<-solve                 ##   and caches it as the variable i.
        getinverse <- function() i          ## This function simply returns the
                                            ##   cached inverse (i).
        list(set = set, get = get,          ## Creates a list of the functions
             setinverse = setinverse,       ##   defined within this environment.
             getinverse = getinverse)       
}


## This function computes the inverse of the matrix returned by the
## makeCacheMatrix function above. If the inverse has already been calculated
## (and the matrix has not changed), then the cacheSolve function retrieves the
## inverse from the cache.

cacheSolve <- function(x, ...) {            ## Returns a matrix that is the
                                            ##   inverse of 'x'
        i <- x$getinverse()                 ## Retrieves the cached inverse
                                            ##   defined in makeCacheMatrix.
        if(!is.null(i)) {                   ## If i is defined (not NULL), 
                message("getting cached data") ## it prints the message and
                return(i)                      ## returns i.
        }
        data <- x$get()                     ## If i is NULL, then this assigns
                                            ##   the matrix from makeCacheMatrix 
                                            ##   to the variable 'data.'
        i <- solve(data, ...)               ## Finds the inverse of matrix x,
        x$setinverse(i)                     ##   caches this new value of i        
        i                                   ##   and prints it. 
}
