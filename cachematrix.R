## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Modified example makeVector function to accomodate matrices instead. 
## Found a loophole, when calling a$setinv(inv) or a$setmean(mean) in the original function, you can
## basically input any value and the cache will print with no error. For example, I set the inverse of
## a 2X2 matrix to a 3X3 matrix using a$setinv(inv). Running cacheSolve(a) printed the 3X3 with no error.


makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Write a short comment describing this function

## Modified exaple cacheMean function using the solve function instead, to accomodate matrix input.

cacheSolve <- function(x, ...) {    
  
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  m <- solve(data, ...)
  
  x$setinv(m)
  
  m
  
}
