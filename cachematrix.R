## These functions cache the inverse of a matrix for later use,
## to limit the need for costly matrix inversion comuptations


## makeCacheMatrix function creates a special matrix object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list (set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

## cacheSolve function computes the inverse of the special matrix 
## from makeCacheMatrix; returning info from a cache if matrix is
## unchanged and data has already been solved for, or solving for
## new data

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}