## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## x is an invertable square function
## The function does the following
##    Sets the matrix
##    Gets the matrix
##    Sets the inverse of the matrix
##    Gets the inverse of the matrix
##    Return a list used by cacheSolve to either solve for the inverse
##    Or retreive a cached inverse


makeCacheMatrix <- function(x = matrix()) {
  
  i = NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Write a short comment describing this function

## x is list of MakeCacheMatrix output
## If the inverse of the matrix is already cached (and has not been modified), return the inverse
## If the inverse of the matrix is NOT cached, calculate and return the inverse
## The solve() function is used for the inverse calculation


cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("Getting cached data.")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
  
}
