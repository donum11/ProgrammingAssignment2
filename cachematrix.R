makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse as NULL

  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset inverse when matrix changes
  }

  get <- function() x

  setInverse <- function(inverse) inv <<- inverse

  getInverse <- function() inv

  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  if (!is.null(inv)) {
    message("Getting cached data")
    return(inv)  # Return cached inverse
  }

  mat <- x$get()
  inv <- solve(mat, ...)  # Compute inverse
  x$setInverse(inv)       # Cache the inverse
  inv
}


# Test code
mat <- matrix(c(4, 7, 2, 6), 2, 2)
cachedMatrix <- makeCacheMatrix(mat)
cacheSolve(cachedMatrix)  # First time: computes
cacheSolve(cachedMatrix)  # Second time: retrieves cached
