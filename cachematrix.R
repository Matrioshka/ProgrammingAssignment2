## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y ## assign a 'global' value to an x 
      m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
  
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## first checks to see if the inverse has already been calculated.
  ## If so, get the inverse from the cache and skips the computation.
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m) ## Return a matrix that is the inverse of 'x'
  }
  ## Else, calculate the inverse of x and set the value of
  ## the inverse in the cache via the solve function.
  data <- x$get()
  m <- solve(data, ...)
  
  x$setsolve(m)
  m
}
