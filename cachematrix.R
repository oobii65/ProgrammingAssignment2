## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##
## Use the R lexical scope and the <<- operator to
## to make a variabel in the "parent" environment that 
## can be used to hold a cached version of the inverse 
## of a matrix. 
##
##
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
##
## Returns the inverse of a matrix that is declared using the 
## makeCacheMatrix(...) function.
## When an inverse is computed it is cached in the "parent" environment 
## using the <<- operator.
## If a cached version exists, this will be returned without performing
## any computations.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)    
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
