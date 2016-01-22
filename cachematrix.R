##This assignment is to write a pair of functions that cache the inverse of a matrix.rather than computing it repeatedly.
##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(get = get,
       setinv = setinv,
       getinv = getinv)
}


## The following function calculates the inverse of the matrix created with the above function. 
## However, it first checks to see if the inverse has already been computed. If so, it gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it computes the inverse matrix and assigns the inverse matrix in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
  ## Return a matrix that is the inverse of 'x'
}