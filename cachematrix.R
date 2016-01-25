## The first function, makeCacheMatrix creates a special "matrix", which is 
## really a list containing a function to

## set the matrix
## get the matrix
## set the inverse of the matrix
## get the inverse of the matrix


## this function creats and store a matrix object and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(solve) inv <<- solve
  getInv <- function() inv
  
  list(set = set, get = get, setInv = setInv, getInv = getInv)
  
}


## Write a short comment describing this function

## The following function calculates the inverse of the special "matrix" created 
## with the above function. However, it first checks to see if the Inverse has
## already been calculated. If so, it gets the inverse from the cache and skips 
## the computation. Otherwise, it calculates the inverse of the matrix and sets 
## the value of the inverse in the cache via the setInv function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
  
}
