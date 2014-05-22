## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## Set the inverse to null just in case
        ## an empty instace is asked to return inverse
        inv <- NULL
        
        ## Set the instance of makeCacheMatrix with new
        ## Matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## Return the newly created matrix
        get <- function() x
        
        ## Store the the inverse of the matrix
        setinverse <- function(inverse) inv <<- inverse
        
        ## Get the stored inverse of the matrix
        getinverse <- function() inv
        
        ## Return a list of methods
        list(set = set, get = get, setinverse = setinverse,
             getinverse = getinverse)
        
}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
        ## get the inverse of the instace of makeCacheMatrix
        inv <- x$getinverse()
        
        ## Check to see if the makeCacheMatrix instace has 
        ## a value and if so, inform the user and return the
        ## value and disregard the rest of the code
        if(!is.null(inv)) {
                message("Getting cached inverse")
                return(inv)
        }
        
        ## The value was null and so we need to set and store it
        data <- x$get() # get the matrix from parent environment
        inv <- solve(data, ...) # calculate the inverse 
        x$setinverse(inv) # cache the inverse to parent environment
        inv  # now return the computed inverse   
}
