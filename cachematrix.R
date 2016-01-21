## Caching the inverse of a matrix
## The 2 functions below are used to calculate the inverse of a matrix.
## If the matrix has been cached before, the inverse of the matrix will be returned rather than recalculated repeatedly.

## makeCacheMatrix creates a special "matrix", which is a list containing a function to:
##    1.  set the value of the matrix
##    2.  get the value of the matrix
##    3.  set the inverse of the matrix
##    4.  get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <-NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }

  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This functions calculates the inverse of the special "matrix" created with makeCacheMatrix function above.
## It checks first whether the inverse of the matrix has already been calculated.
## If so, the calculation is skipped, and the gets the inverse from the cache. 
## Otherwise, the inverse of the matrix is calculated, and set in the cache via setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setInverse(m)
  m
}
