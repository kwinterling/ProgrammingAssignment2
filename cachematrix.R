## makeCacheMatrix() takes a standard R matrix object as its argument
## and returns a "cache matrix" that caches its inverse when its
## inverse is computed with the cacheSolve() function.

## Takes an R matrix object and returns a cache matrix object, a
## list of functions set, get and setinverse, getinverse, which
## set and retrieve the underlying matrix and its inverse, respectively.
## If the matrix's inverse has not been computed using cacheSolve()
## yet, then getinverse() returns NULL.

makeCacheMatrix <- function(x = matrix()) {
  cminv <- NULL
  
  set <- function(y) {
    x <<- y
    cminv <<- NULL
  }
  get <- function() x
  setinverse <- function(cminverse) cminv <<- cminverse
  getinverse <- function() cminv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve computes the inverse of a cache matrix x and caches the inverse.
#if the inverse of x has already been computed and the value of x has not
# changed, then the previously cached inverse is returned. Otherwise, the
# inverse is computed using solve(), cached, and then returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cminv <- x$getinverse()
  
  if (!is.null(cminv)) {
    message("accessing cached data")
    return(cminv)
  }
  
  data <- x$get()
  cminv <- solve(data, ...)
  x$setinverse(cminv)
  cminv
}
