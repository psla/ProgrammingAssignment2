# This function creates a special matrix object, which allows to cache the inverse value, if computed using cacheSolve
# x is the matrix you want to allow to be cached
# Usage example:
# mc <- makeCacheMatrix(hilbert(8))
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
# Usage example:
# mc <- makeCacheMatrix(hilbert(8))
# cacheSolve(mc) # will compute and store inverse in cache
# cacheSolve(mc) # will return value from cache
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
  computedInverse <- solve(x$get(), ...)
  
  # store computed value in cache
  x$setinverse(computedInverse)
  
  # return the computed value
  computedInverse
}

## THIS IS THE END OF ASSIGNMENT; READ BELOW IF YOU WANT TO GIVE ME PERSONAL FEEDBACK ON DIFFERENT APPROACH ##
## (you can shoot me an email luserek at gmail)


### 
### I do not think that suggested proposal of methods is a good one
### The problem I see is that makeCacheMatrix returns a public setters for the cache item
### and everyone who sees this object can change it - even when it won't be good thing to do
### This breaks encapsulation of fields.
### I think that cacheSolve should be part of the makeCacheMatrix and setinverse should not be exposed;
### getinverse should call inverse and cache it (given that the cache already belongs to the matrix)
###
### See the example below:

# Example:
# cm <- cachedMatrix(hilbert(8))
# cm$getinverse() # will compute it
# cm$getinverse() # will return cached value
cachedMatrix <- function(x = matrix()) {
  # initialize the variable which will store the inversed cached value
  # initially do not assign any value (it will be lazily assigned when cacheSolve is called)
  inversedCache <- NULL
  
  # returns the backend matrix
  get <- function() x
  
  # changes the stored matrix to a different one
  # cleares cache
  set <- function(newMatrix) {
    x <<- newMatrix
    inversedCache <<- NULL
  }
  
  # this function now computes inverse if it is yet computed or returns cached value
  # The advantage of this approach is that nobody should be able to change cached value ever; only this method
  # can do it; which guarantees that it corresponds correctly to the matrix itself
  # the logic in this function is the same as the logic in the cacheSolve above, but it is encapsulated
  getinverse <- function() {
    # same logic as earlier: see if we already have cached value, if not, compute and cache it
    if(!is.null(inversedCache)) {
      message("getting cached data")
      return(inversedCache)
    }
    
    # compute
    inversedCache <<- solve(x)
    
    # return computed value
    inversedCache
  }
  
  # returns a 'public API' of this makeCacheMatrix
  # the public API now does not expose setinverse as it is handled internally
  list(set = set, get = get,
       getinverse = getinverse)
}
