## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix creates a matrix  which is a list with the function to 1) set the value of the matrix
# 2) get the value of the matrix, 3) set the value of the inverse, 4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}


## The cacheSolve function checks to see if the inverse has already been calculated.  if so, it skips recalcuating and
## gets the value from the cache.  Otherwise, it will calculate the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
