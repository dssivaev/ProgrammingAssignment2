## Functions return an inverse of matrix, first it checks if an inverse
## was already stored in cache, if not it calculates it

## makeCacheMatrix creates a list of function to set the matrix,
## get the matrix, set the mean, and get the mean

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- matrix()
  }
  get <- function() x
  setinv <- function(inverse)  inv<<- inverse
  getinv <- function() inv
  list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve checs if the inverse of the matrix has been stored in 
## cache already, if not it calculates the inverss,
## in the end it returns the inverse matrix

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
