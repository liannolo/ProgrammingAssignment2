

## This set of functions instantiates a matrix object
## and then finds the inverse of that matrix

## This function instantiates a makeCacheMatrix object that has
## four member functions:
## two setter and two getter functions
## to set and get the matrix and to set and get the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL #clears prior execution of cacheSolve
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function looks for a cache of the inverse
## of the matrix and if one exists, it returns the cache
## otherwise, it computes the inverse and returns it
## to the environment

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) { # Look to see if the inverse is cached and if so retrieve the data
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  return(m) ## Return a matrix that is the inverse of 'x'
  
}
