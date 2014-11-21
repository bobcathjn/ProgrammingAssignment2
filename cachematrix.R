## My functions here can cache the inverse of a matrix so that it won't be recomputed again when needed.

## This following function, makeCacheMatrix, creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## This following function returns the inverse of the matrix. First, it checks if
## the inverse has already been computed. If so, it gets the result and skips the
## computation. If not, it computes the inverse of the matrix and sets the value in the cache.
cacheSolve <- function(a, ...) {
  inverse <- a$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data.")
    return(inverse)
  }
  data <- a$get()
  inverse <- solve(data)
  a$setinverse(inverse)
  inverse
}
