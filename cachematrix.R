## 2 functions, first one to make matrix with getter setter

## get a matrix and make it gettable, settable

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

## get makeCacheMatrix result, calculate inverse if it is not already calculated
## return the inverse of matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  else { message("calculating")}
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
