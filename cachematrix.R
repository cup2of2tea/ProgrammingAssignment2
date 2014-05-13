## CTRL+A CTRL+C CTRL+V

## we initialize functions used to set and get matrix i
## and functions used to set and get inverse
## Initially, the inverse matrix is not calculated, so i has null value

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## if the inverse was already calculated and the matrix is not changed
## we simply returns the precalculated inverse
## Else we calculate it with function solve

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(x$get(), ...)
  x$setinverse(i)
  i
}