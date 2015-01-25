## These functions solve for the inverse of an arbitrary matrix,
## but caches the result to save computation time if an identical
## matrix is called to it.

## This function should create a cache of the matrix should the cacheSolve
## function require it to do so.
makeCacheMatrix <- function(x = matrix()) {
  ## i gets intialized to null, but will eventually hold
  ## the inverse matrix object
  i <- NULL
  ## set is intialized with an anonymous function which
  ## will assign x globally as the stored matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ## calls the anynomous function and assigns it to get
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function () i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
 
}


## cacheSolve, given a matrix x, should return the inverse of the matrix (stored in i)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x[getinverse]
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x[get]
  m <- solve(data, ...)
  x[setinverse(i)]
  i
  
}
