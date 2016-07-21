## from the assignment discription:
## makeCacheMatrix: This function creates a
## special "matrix" object that can cache its inverse.
##
## cacheSolve: This function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix above. If
## the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve 
## the inverse from the cache.


## The following function has four parts:
## set, get, setinverse, and getinverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  ##setmean <- function(mean) m <<- mean
  setinverse <- function(inverse) m <<- inverse
  ##getmean <- function() m
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## I've only found solve function for computing inverse
## seems it has some limit
## all other inverse matrix function require install other packages

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  ##m <- mean(data, ...)
  m <- solve(data, ...)
  ##x$setmean(m)
  x$setinverse(m)
  m
}
