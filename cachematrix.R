## These functions allow the creation of a function that contains a matrix, and allow
## the inverse of the matrix to be cached once it has been calculated in order to save the time
## required to calculate it over and over again.

## This is the function used to set up the CacheMatrix, and creates the functions needed
## to obtain and modify the matrix and inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL     ## Set inverse to NULL as it has not yet been calculated
        
        ## Function used to set matrix if makeCacheMatrix is called without a matrix as argument, or change matrix later
        set <- function(y) {    
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x     ## Return matrix
        
        setinv <- function(inverse) {inv <<- inverse}   ## set inv equal to argument passed as inverse
        getinv <- function() inv        ## Return inverse
        
        # Return list of functions
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This funtion returns the inverse of the matrix in the x list object. 
## The first time the inverse is calculated, but it is then 'cached' in 
## x using the setinv() function.

cacheSolve <- function(x, ...) {
        
        # Get inverse from x
        inv <- x$getinv()
        # Check if inverse has been cached
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        # If inverse not cached
        data <- x$get()           # get matrix from x
        inv <- solve(data, ...)   # calculate inverse
        x$setinv(inv)             # save inverse back to x
        inv                       # Return the inverse
}
