## Function creates a special "matrix" object that can cache its inverse.
## Object functions:
##   -setMatrix: set the original matrix, inverserMatrix is set to null to clean the cache.
##   -getMatrix: return the original matrix.
##   -setInverseOfMatrix: set the inverse of matrix to cache. Cached til a new original value is set.
##   -getInverseOfMatrix: return the inverse of matrix if it exist on cache otherwise NULL.

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  
  setMatrix <- function(matrix) {
    x <<- matrix
    inverseMatrix <<- NULL
  }
  
  getMatrix <- function() x
  
  setInverseOfMatrix <- function(inverse) inverseMatrix <<- inverse
  
  getInverseOfMatrix <- function() inverseMatrix
  
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setInverseOfMatrix = setInverseOfMatrix,
       getInverseOfMatrix = getInverseOfMatrix)
}

## CacheSolve receive as argument a makeCacheMatrix object
## This auxiliar function is meant to return the cached value if it exist 
## otherwise it will get the original matrix from MakeCacheMatrix
## calculate the inverse, set to the cache and return it.

cacheSolve <- function(x, ...) {
  cachedInverseMatrix <- x$getInverseOfMatrix()
  
  if (!is.null(cachedInverseMatrix)) {
    message("getting cached data")
    return(cachedInverseMatrix)  
  
  } else {
    matrix <- x$getMatrix()
    inverseMatrix <- solve(matrix)
    x$setInverseOfMatrix(inverseMatrix)
    
    inverseMatrix
  }  
}
