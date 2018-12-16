## Below are two functions that are used together to cache the inverse of a matrix. 

## The function makeCacheMatrix() creates an R object that stores a matrix and its inverse.
## The object returned by this function will then be used by the cacheSolve() function.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {  
                x <<- y             
                m <<- NULL    
        }
        get <- function() x  
        setmatrix <- function(solve) m <<- solve    
        getmatrix <- function() m    
        list(set = set, get = get,  
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}                                      


## The function cacheSolve() requires an argument that is returned by makeCacheMatrix().
## This function retrieves the inverse from the cached value that is stored in the 
## makeCacheMatrix() object's environment.

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m               ## Return a matrix that is the inverse of 'x'
}
