##These two functions create a method for caching the inverse of a given matrix
##to avoid repeated calculation. 

## This function takes a matrix ("x") as an argument, and returns a list with four 
## functions as elements. These functions are accessed with via the '$' notation (ie 'x$set()), 
## and are used to manipulate the matrix, and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        I <- NULL
        
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        
        get <- function() x
        
        
        setinverse <- function(inverse) I <<- inverse
        
        getinverse <- function() I
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        

}


## This function takes the object created by makeCacheMatrix and returns the inverse  of the matrix,
## first by checking if the inverse has already computed; if not, the inverse is calculated,
## and the makeCacheMatrix object is updated with the inverse value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        I <- x$getinverse()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- x$get()
        I <- solve(data)
        x$setinverse(I)
        I
}
