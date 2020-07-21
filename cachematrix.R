## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix creates a list of vectors to functions.  These functions
## access a cache location for a matrix and it's inverse.
## 
## We store the matrix and its inverse in the parent environment of these
## function definitions using the <<- operator. This 'Parent Environment' is
## normally the Global Environment. 
## this function builds a set of functions and returns the functions within a list to the parent environment. 
## The result is an object "makeCacheMatrix" which contains 
## four function: set(), get(), setinverse(), getinverse().
## 

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
  set <- function(y) {
          x <<- y
          i <<- NULL
  }
## the get() use lexical scoping and retrives the x from the parent environment because it is outside the get function.
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## retreive the cached matrix inverse. it will save comutation time because it will check if inverse is already calculated
## if inverse doesnt exist, it will calcuate it and send the results to the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
          message("getting cached data")
          return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
