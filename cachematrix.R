## There are a pair of functions that cache the inverse of a matrix. 
## Matrix is squered, invertible.

## function "makeCacheMatrix" creates a special "matrix", 
## which is really a list containing a function to
## 1) set the matrix
## 2) get the matrix
## 3) set the inverse matrix
## 4) get the the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(Solve) m <<- Solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## "cacheSolve" function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
  
  ## Return a matrix that is the inverse of 'x'
}
