## This pair of functions creates a matrix able to cache its inverse
## and computes its inverse if not already computed

## This function creates a new matrix that is able to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) im <<- inverse
  getinverse <<- function() im
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the matrix created by 
## makeCacheMatrix, unless the inverse has already been executed, 
## in which case it will be retrieved

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  im <- x$getinverse()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  mat <- x$get()
  im <- solve(mat, ...)
  x$setinverse(im)
  im
}