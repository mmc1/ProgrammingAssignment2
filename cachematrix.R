## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatix is a function that creates a list containing
# functions that:
# 1. sets the value of an invertible matrix. Note that it is
#    assumed that the matrix is invertible.
# 2. gets the value of the invertible matrix. 
# 3. sets the value of the inverse of the matrix.
# 4. gets the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set.inverse <- function(solve) m <<- solve
  get.inverse <- function() m
  list(set = set, get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse)
}

## Write a short comment describing this function
# This function calculates the inverse of the special matrix
# that was created with 'makeCacheMatrix. If the inverse
# has already been calculated, then it gets the cached value
# of the inverse and reports this along with a message. If the
# inverse has not been calculated, then it is calculated with
# the function solve()

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$get.inverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set.inverse(m)
  m
}
