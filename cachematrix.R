## File Includes 2 R functions
## 
## Functions included in this file: 
## 1. makeCacheMatrix
## 2. cacheSolve

## The pair of functions are intended to create a special 
## "matrix" and cache its inverse. If the inverse has
## already been calculated and the matrix has not changed,
## the second function will return the inverse from cache.
## Else, it will recalculate.


## Define makeCacheMatrix function
## Creates a special "matrix" object and cache inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Define cacheSolve function
## Compute or return cached inverse of special "matrix"
## created by makeCacheMatrix function defined above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
              message("getting cached data")
              return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}