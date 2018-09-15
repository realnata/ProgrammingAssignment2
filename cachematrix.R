## In the assignment I create a functions that count inverse matrix and 
## values of matrix and inverse matrix write in Cache.

##The  function, makeCacheMatrix creates a matrix, which is really 
##a list containing a function to set and get the value of the matrix and
##set and get the value of the inverse of matrix (solve)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## In the function calculates 
## the solve of the matrix created with the above function. 

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
