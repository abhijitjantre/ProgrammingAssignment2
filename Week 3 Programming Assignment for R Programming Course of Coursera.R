## Following function creates a special "matrix" object that can cache its inverse ##

makeCacheMatrix <- function(a = matrix()) {
      inv <- NULL
      set <- function(b) {
            a <<- b
            inv <<- NULL
      }
      get <- function() a
      setInverse <- function(inverse) inv <<- inverse
      getInverse <- function() inv
      list(set = set,
           get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}

## Following function computes the inverse of above special "matrix" returned by makeCacheMatrix ##

cacheSolve <- function(a, ...) {
      ## Return a matrix that is the inverse of 'a'
      inv <- a$getInverse()
      if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      mat <- a$get()
      inv <- solve(mat, ...)
      a$setInverse(inv)
      inv
}
