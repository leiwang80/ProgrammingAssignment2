## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  cacheSolve <- NULL
  set <- function(y) {
    x <<- y
    cacheSolve <<- NULL
  }
  get <- function() x
  setSolve <- function(inverse) cacheSolve <<- inverse
  getSolve <- function() cacheSolve
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cacheSolve <- x$getSolve()
  if(!is.null(cacheSolve)) {
    message("getting cached data")
    return(cacheSolve)
  }
  matrix <- x$get()
  cacheSolve <- solve(matrix, ...)
  x$setSolve(cacheSolve)
  cacheSolve
}
