## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix is an R class containing functions that take
##   a matrix, store it in the function, and return the inverse
##   of that matrix

makeCacheMatrix <- function(x = matrix()) {
    invmatrix <- NULL
    
    # Set the value of the matrix
    set <- function(y) {
        x <<- y
        invmatrix <<- NULL
    }
    
    # Get the value of the matrix
    get <- function() x
    
    # Set the value of the inverse matrix using the solve function
    setmatrix <- function(solve) invmatrix <<- solve
    
    # Get the value of the inverse matrix
    getmatrix <- function() invmatrix
    
    # Return the matrix and the defined functions
    list(set = set, get = get,
           setmatrix = setmatrix,
           getmatrix = getmatrix)

}


## Write a short comment describing this function

## cacheSolve takes a matrix and checks if the inverse has been
##   calculated. If calculated, it pulls it from the cache, and
##   if not performs the inverse calculated via the solve
##   function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invmatrix <- x$getmatrix()
    
    # Check the cache; if present return the inverse matrix
    #   and exit function
    if(!is.null(invmatrix)) {
        message("getting cached data")
        return(invmatrix)
    }
    
    # Calculate and return the inverse matrix via the solve
    #   function
    data <- x$get()
    invmatrix <- solve(data, ...)
    x$setmatrix(invmatrix)
    invmatrix
}
