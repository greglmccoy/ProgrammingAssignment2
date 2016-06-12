## Function set to calculate the inverse of a matrix for Coursera R Programming Week 3 Assignment.

## Write a short comment describing this function
# The function 'makeCacheMatrix' returns a list with functions 'set', 'get'
# 'setinverse' and 'getinverse'.  

makeCacheMatrix <- function(x = matrix()) {
## This function creates a "matrix" that can cache its inverse
 
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL 
    set <- function(y) { 
        x <<- y  
        inv <<- NULL
    }
    get <- function() x
    
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function
# The function "cacheSolve" returns the inverse of the matrix. It first checks if
# the inverse has already been computed, and returns the result if so.  If not, 
# it computes the inverse, and sets the value using 'setinverse' function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
