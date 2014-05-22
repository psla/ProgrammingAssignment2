## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function creates a special matrix object, which inverse will be cached, if computed using cacheSolve
# x is the matrix you want to allow to be cached
makeCacheMatrix <- function(x = matrix()) {
  # initialize the variable which will store the inversed cached value
  # initially do not assign any value (it will be lazily assigned when cacheSolve is called)
  inversedCache <- NULL
  
  # changes the stored matrix to a different one
  # cleares cache
  set <- function(newMatrix) {
    x <<- newMatrix
    inversedCache <<- NULL
  }
  
  # returns the backend matrix
  get <- function() x
  
  # returns the cached inverse. Cached inverse will be NULL if it has
  # not been computed yet
  getinverse <- function() inversedCache
  
  # sets the cached inverse
  setinverse <- function(inverse) inversedCache <<- inverse
  
  # returns a 'public API' of this makeCacheMatrix
  list(set = set, get = get,
       getinverse = getinverse,
       setinverse = setinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
