## This function is used to create a cache for the inverse of the input matrix

## This function creates a special matrix and has 4 function to set, get the matrix
## as well as to get and set the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  get <- function() x
  setinv <- function(inv) invmat <<- inv
  getinv <- function() invmat
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by
## the makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cacheSolve retrieves the
## inverse from cache.


cacheSolve <- function(x, ...) {
  invmat <- x$getinv()
  if(!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }
  data <- x$get()
  invmat <- solve(data, ...)
  x$setinv(invmat)
  invmat
}
