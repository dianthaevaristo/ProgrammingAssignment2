##makeCacheMatrix is the function that creates a matrix and cache its inverse solved using the function cacheSolve
makeCacheMatrix <- function(x = matrix()) {
  #varibale 'm' stores the value of the inverse matrix
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ##for getting the matrix
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  ## for getting the inverse
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}
##cacheSolve computes for the inverse and return its values
cacheSolve <- function(x, ...) {
  m <- x$getinv()
        ##This is when the inverse is not yet computed
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##computation of the inverse
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
