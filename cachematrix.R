## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        ## Initialize inverse property
        inv <- NULL

        ## Setter method
        set <- function(matrix) {
                x <<- matrix
                inv <<- NULL ## reset cached inverse when matrix changes
        }

        # Getter method
        get <- function() {
                x
        }
        
        ## Setter method for inverse
        setInverse <- function(inverse) {
                inv <<- inverse
        }

        ## Getter method for inverse
        getInverse <- function() {
                inv
        }

        ## Return list of methods
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## If the inverse has already been computed and not changedthen "cacheSolve" returns the already cached inverse 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        m <- x$getInverse()
        if (!isNull(m)) {
                ## Return cached matrix if already set
                return(m)
        }

        ## Otherwise get, compute and cache the inverse matrix
        
        data <- x$get() 
        
        ## Compute inverse matrix using the "solve" function
        m <- solve(data)

        ## Cache inverse matrix
        x$setInverse(m)

        ## Return computed inverse matrix
        m
}
