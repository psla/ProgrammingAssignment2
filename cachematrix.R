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


# This function returns an inverse of the matrix. If the inverse is not already
# computed, it will compute it and store in cache; otherwise, it will just return the value from cache
# x must be the output of the makeCacheMatrix function
cacheSolve <- function(x, ...) {
  # try to get cached value
  inverse <- x$getinverse()
  
  # verify if inverse has ever been cached (NULL indicates it has not been computed yet)
  # if it has been computed; return cached value
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  # otherwise, compute the inverse and store in cache
  computedInverse <- solve(x$get())
  
  # store computed value in cache
  x$setinverse(computedInverse)
  
  # return the value
  computedInverse
}
