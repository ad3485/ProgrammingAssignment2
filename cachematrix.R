## the code overall creates a matrix and caches the inverse of it

## the function creates a matrix x

makeCacheMatrix <- function(x = matrix()) {
        invrs <- NULL
        set <- function(y) {
                x <<- y
                invrs <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) invrs <<- inverse
        getInverse <- function() invrs
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



##the function finds the inverse of the matrix x.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invrs <- x$getInverse()
  if(!is.null(invrs)) {
          message("getting cached data")
          return(invrs)
  }
  matrx <- x$get()
  invrs <- solve(matrx, ...)
  x$setInverse(invrs)
  invrs
}