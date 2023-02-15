## Based on the makeVector and cachemean examples, these are two functions that are used 
## to compute the inverse of a square matrix. 
## makeCacheMatrix has four functions: set, get, setinverse and getinverse. 
## Assuming that x is a square invertible matrix. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cachSolve to compute the inverse of matrix returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## the cachesolve should retrieve the inverse from the cache. 

cacheSolve <- function(x) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}

##Example
matrix_1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
solve(matrix_1)
myMatrix_object <- makeCacheMatrix(matrix_1)
cacheSolve(myMatrix_object)
cacheSolve(myMatrix_object)