## Function to create, retrieve and cache inverted matrices

## Function to encapsulate the Matrix data - source matrix and inverson of the source matrix
makeCacheMatrix <- function(source.matrix = matrix()) {
  ## Validate the source
  if (!is.matrix(source.matrix)) {
    stop("Function accepts only invertible matrix as input")
  }
  ## Initialize the inverted
  inverted.matrix <- NULL
  ## Set the source matrix
  set <- function(y) {
    source.matrix <<- y
    inverted.matrix <<- NULL
  }
  ## function to retrieve the source matrix
  get <- function() source.matrix
  ## function to set the inverted matrix
  setInverse <- function(inverse) inverted.matrix <<- inverse
  ## function to retrieve the inverted matrix
  getInverse <- function() inverted.matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Function takes an invertible matrix as input x. 
## If the inverse of the input matrix is already cached, function returns cached inverted matrix of the input x
## If the inverse of the input matrix is not cached yet, function calculates the inverted matrix and sets it to the input x
cacheSolve <- function(x, ...) {
  inverted.matrix <- x$getInverse()
  ## Check the cache
  if(!is.null(inverted.matrix)) {
    message("getting cached data")
    return(inverted.matrix)
  }
  ## inverse of matrix is not cached yet. Create the inverse and sets it to the input x
  source.matrix <- x$get()
  inverted.matrix <- solve(source.matrix, ...)
  x$setInverse(inverted.matrix)
  inverted.matrix
}
