## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
   inverse <- NULL
   set <- function(x) {
      x <<- x;
      inverse <<- NULL;
   }
   get <- function() return(x);
   setinv <- function(inv) inverse <<- inv;
   getinv <- function() return(inverse);
   
   # set the value of the vector
   # get the value of the vector
   # set the value of the mean
   # get the value of the mean
   # could have used list instead of return
   return(list(set = set, get = get, setinv = setinv, getinv = getinv)) 
}


## Write a short comment describing this function
# This function computes the inverse of the special "matrix" returned 
# by the function makeCacheMatrix. If the inverse has already been
# calculated (and the matrix has not changed), then cacheSolve should
# retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
   
    inverse <- x$getinv()  # checks to see if already inversed
    if(!is.null(inverse)) {
        message("Getting cached data...")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)  # returns the inverse
    x$setinv(inverse)
    return(inverse)
}
