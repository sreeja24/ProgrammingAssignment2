## Overall description of functions:
# The function creates a cache to save time for computing matrix inverse if it has already been computed

## Comments on makeCacheMatrix below:
# x is evaluated "lazily", it happens when get() is invoked
# makeCacheMatrix does not need to be called everytime, modifying only the set function will work fine.
# Run example below:
# a<-makeCacheMatrix(mt)
# mt = cbind(c(1,0,5),c(2,1,6),c(3,4,0))
# cacheSolve(a)
# Another execution can discard the 1st 2 steps (optional) and use the following 2 steps:
# a$set(cbind(c(2,2),c(3,2)))
# cacheSolve(a)

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
      x <<- y
      i <<- NULL
  }
  get <- function() x 
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Comments on cacheSolve below:
# This function computes the inverse of the special "matrix" 
#returned by makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve should retrieve the 
#inverse from the cache

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  
}

