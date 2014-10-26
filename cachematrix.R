## Put comments here that give an overall description of what your
## functions do

## The first function takes a symmetrical matrix as the input and outputs a list that is a cache of the matrix
## The second function takes the first as the input argument, and outputs the inverse of the matrix.
## The second fuction stores the inverse for future recall, and updates this value in the first function
## using the <<- operator


## Write a short comment describing this function

## Modified example makeVector function to accomodate matrices instead.
## In this function, a symmetrical input matrix is manipulated within a list of sub-functions to create a cache.
## The <<- assignment operator is used to upate the matrix inverse calculated in cacheSolve into makeCacheMatrix.

## Included an if loop to check the matrix dimensions and ensure symmetry in the input data.
## Will output an error message in case of asymmetrical data.

makeCacheMatrix <- function(x = matrix()) {
if (nrow(x) == ncol(x)){
  
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
else {message("Invalid matrix argument. Number of rows are not equal to number of columns")}
}


## Write a short comment describing this function

## Modified exaple cacheMean function using the solve function instead, to accomodate matrix input.
## This function returns the inverse matrix of the original argument, and takes the makeCacheMatrix function
## as the input argument. The use of the solve functionis minimized by the use of the if loop. This saves
## time as the solve function is relatively complicated.


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
  
  ## Found a loophole, when calling a$setinv(inv), you can basically input any value and the cache
  ## will print with no error. For example, I set the inverse of a 2X2 matrix to a 3X3 matrix using
  ## a$setinv(inv). Running cacheSolve(a) printed the 3X3 with no error.
  
  m

}
