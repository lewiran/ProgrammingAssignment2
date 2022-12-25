## Create a Matrix object that contains a field for its inverse.
## functionality to initialize matrix, assign values to the matrix, 
## get the values of the matrix, calculate and store the inverse
## of the matrix.

## makeCacheMatrix initializes a matrix and has a field for storing the inverse,
## and has functions to assign values to the matrix, get the values of the matrix,
## assign the inverse, and retrieve the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function() {
      x
  }
  set <- function(y = matrix()) {
      x <<- y
      inv <<- NULL
  }
  getInverse <- function() {
      inv
  }
  setInverse <- function(y = matrix()) {
      inv <<- y
  }
  list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}


## cacheSolve returnse the Inverse of matrix object x, where x
## was initialized by makeCacheMatrix.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
        ## Return a matrix that is the inverse of 'x'
  if (is.null(inv)) {
      data <- x$get()
      inv <- solve(data, ...)
      x$setInverse(inv)
      inv
  }
  else {
      message("using cached data")
      inv
  }
}
