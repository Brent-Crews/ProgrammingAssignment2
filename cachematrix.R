## Part 1: makeCacheMatrix
## create a matrix that is fully 
## capable of caching its own inverse
## this function will run in conjunction 
## with the cacheSolve Matrix afterwards

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Part 2: cacheSolve function
## execute the inverse of the
## matrix, that was created in part 1
## (makeCacheMatrix). If for some reason 
## the inverse was already processed,
## then this function will automatically
## obtain the inverse from cache memory
## and will create a comment, which
## will serve as a demarkation
## to know that that branch of code
## was reached
cacheSolve <- function(x, ...) {
  invrs <- x$getInverse()
  if (!is.null(invrs)) {
    message("Let's Get Cached Data")
    return(invrs)
  }
  matrx <- x$get()
  invrs <- solve(matrx, ...)
  x$setInverse(invrs)
  invrs
}
