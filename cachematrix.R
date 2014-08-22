## Function takes matrix as argument and returns special object
## wich provides access to this matrix and it's inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  get <- function() x
  set <- function(m) {
    x <<- m
    i <<- NULL
  }
  getInverse <- function() inverse 
  setInverse <- function(i) inverse <<- i
  list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)
}


## Function takes special object with matrix inside as
## an argument and returns inverse matrix, if available returns cached version
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  m<-x$get()
  inverse <- solve(m, ...)
  x$setInverse(inverse)
  inverse
}
