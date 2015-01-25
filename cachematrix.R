## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object
## that can cache its inverse. The special matrix is
## really a list containing a function to:
##
## 1.  set the value of the vector
## 2.  get the value of the vector
## 3.  set the value of the mean
## 4.  get the value of the mean. 
## 

makeCacheMatrix <- function(x = matrix()) {
        ## The inverse matrix initialised as Null
        m <- NULL
        
        ## Save data and inverse matrix to cache
        set <- function(y) {
          x <<- y
          m <<- NULL
        }
        
        ## Return saved matrix
        get <- function() x
        
        ## Save inverse matrix to cache
        setsolve <- function(solve) m <<- solve
        
        ## Return inverse matrix from cache
        getsolve <- function() m
        
        ## Create the special matrix
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## The following function calculates the the inverse of a square matrix
## using the `solve` function in R. 
## The input matrix is a special matrix created with "makeCacheMatrix"
## function.
## In case the inverse matrix has been computed again, it is returned
## from a cached copy, else it is computed, saved to cache and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Get previous solution
        m <- x$getsolve()
        
        ## If the inverse has been computed return it from cache
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        
        ## Compute the inverse matrix if not saved in cache
        ## Get matrix data
        data <- x$get()
        
        ## Use the internal solve R function
        m <- solve(data, ...)
        
        ## Save the inverted matrix to cache
        x$setsolve(m)
        
        ## Return the inverted matrix
        m
}
