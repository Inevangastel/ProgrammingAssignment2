## The function <<- assigns a value to an object that is used in an external environment (not your own environment).
## The makeCacheMatrix function creates an object to store a matrix and its inverse.
## The functions get() and set() are used to store a matrix and it's cache mean.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        set_inversie <- function(inv) m <<- inv
        get_inversie <- function() m
        
        list(set = set, get = get,
                set_inversie = set_inversie,
                get_inversie = get_inversie)
}

## The cacheSolve function takes a special matrix object which is created with the makeCacheMatrix.
## Then it returns the cached inverse if the matrix hasn't changed, or recomputes the inverse
## if the matrix has been changed.
## The function solve() computes the inverse of a square matrix (the matrix in this assignment is a square matrix).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$get_inversie()
        
        if(!is.null(inv)) {
                message("getting cached inverted matrix")
                return(inv)
        }
        
        inv <- solve(x$get())
        x$set_inversie(inv)
        inv
}

