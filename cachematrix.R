## The functions computes and cache the inverse of a matrix

## Creates a matrix and cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {                         # changes the matrix stored in the main function
                x <<- y
                m <<- NULL
        }
        get <- function() x                          # returns x that is stored in the main function
        setinverse <- function(solve) m <<- solve    # store the value of the input in a variable m
        getinverse <- function() m                   # return the value of m
        list(set = set, get = get,                   # assign makeCacheMatrix to an object with the 4 functions
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## Computes the inverse of the matrix returned from makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()                          # verify the value m, (exists and not NULL). 
        if(!is.null(m)) {                            # If in memory, returns a message and the value m.
                message("getting cached data")
                return(m)
        }
        data <- x$get()                             # If not in memory, data gets the vector stored with makeCacheMatrix 
        m <- solve(data, ...)                       # calculates the inverse of the vector 
        x$setinverse(m)                             # stores it in the object generated assigned with makeCacheMatrix
        m
}
