makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL
  set <- function(y) {
    x <<- y
    inverse_matrix <<- NULL
  }
  get <- function() x
  setInverse <- function() inverse_matrix <<- solve(x)
  getInverse <- function() inverse_matrix
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  inverse_matrix <- x$getInverse()
  if (!is.null(inv)) {
    message("cached matrix")
    return(inverse_matrix)
  }
  mat <- x$get()
  inverse_matrix <- solve(mat, ...)
  x$setInverse(inverse_matrix)
  inverse_matrix
}
