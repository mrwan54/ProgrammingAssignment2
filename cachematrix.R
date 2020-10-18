# Marwan Kamal - Assigment week 3 - R programming
# Lexical Scoping

# Matrix inversion is usually a costly computation and there may be some 
# benefit to caching the inverse of a matrix rather than compute it repeatedly.
# Below are a pair of functions that cache the inverse of a matrix.
# It is assumed that the matrix supplied is always invertible.

# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverseMat <- NULL
  set <- function(y) {
    x <<- y
    inverseMat <<- NULL
  }
  get <- function() x
  setInverse <- function(rev) inverseMat <<- rev
  getInverse <- function() inverseMat
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# This function computes the inverse of the special "matrix" returned
# by makeCacheMatrix above. If the inverse has already been calculated
# (and the matrix has not changed), then the cachesolve should
# retrieve the inverse from the cache.
# Assumption - the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  inverseMat <- x$getInverse()
  if (!is.null(inverseMat)) {
    message("getting cached data")
    return(inverseMat)
  }
  matr <- x$get()
  inverseMat <- solve(matr, ...)
  x$setInverse(inverseMat)
  inverseMat
}