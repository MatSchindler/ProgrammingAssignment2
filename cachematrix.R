## Author: Mathias Schindler
##
## The function cacheSolve(x, ...) takes a matrix x of type makeCacheMatrix and
## returns the inverse. The inverse is only computed if x has changed since the last
## call of cacheSolve. It is assumed that x is invertable. 

## makeCacheMatrix adds functions to a matrix x that allows the caching of its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        inverseMatrix <- NULL
        
        set <- function(y) {
                x <<- y
                ## reset the cached inverse Matrix
                inverseMatrix <<- NULL
        }
        
        get <- function() x
        
        setInverse <- function(newInverse) inverseMatrix <<- newInverse
        getInverse <- function() inverseMatrix
        
        list(set = set, 
             get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}


## The function cacheSolve(x, ...) takes a matrix x of type makeCacheMatrix and
## returns the inverse.

cacheSolve <- function(x, ...) {
       inverse <- x$getInverse()
       
       ## Case value of x has not changed
       if(!is.null(inverse)) {
               message("Getting cached data!")
               return(inverse)
       }
       
       ## Case value of x has changed
       data <- x$get()
       inverse <- solve(x$get())
       x$setInverse(inverse)
       inverse
}
