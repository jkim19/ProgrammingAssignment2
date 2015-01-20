## jkim19

## This function creates a special "matrix" object that can cache 
## its inverse

makeCacheMatrix <- function(x = matrix()) {
  MI <- NULL
  set <- function(y) {
    x <<- y
    MI <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) MI <<- inverse
  getinverse <- function() MI
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix.  If the inverse has already been calculated (and
## the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  MI <- x$getinverse()
  if(!is.null(MI)) {
    message("getting cached data")
    return(MI)
  }
  data <- x$get()
  MI <- solve(data, ...)
  x$setinverse(MI)
  MI
}
