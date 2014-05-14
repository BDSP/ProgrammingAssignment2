## R-Programming Assignment
## The functions utilize the scoping rules of R 
##  to efficiently determine the inverse of a matrix
##  If a cached result already exists, its value is recalled without recalculation
##
## Structure of the code largely follows the sample code of caching the mean of a vector
##
## Assignment Submitted by BDSP (Xuan Kong)


## Create a special matrix object that is a list of functions 
##  to set/get a value of a matrix
##  to set/get the inverse of the matrix
##
## Assignment Submitted by BDSP (Xuan Kong)

makeCacheMatrix <- function(x = matrix()) {
  # initialize inverse matrix value to NULL
  invm <- NULL
  
  # define setmat as setting new matrix value, resetting invm to NULL
  setmat <- function(y) {
    x <<- y
    invm <<- NULL
  }
  
  # define getmat as getting the matrix value
  getmat <- function() x
  
  # define setinv as storing the inverse matrix to a "global" variable invm
  setinv <- function(solve) invm <<- solve
  
  # define getinv as recalling the calculated value of the inverse matrix
  getinv <- function() invm
  
  # returning the list of "methods" attached to the "global" variable x
  list(setmat = setmat, getmat = getmat, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  # get the inverse, if not previously calculated, it will be NULL
  invm <- x$getinv()
  
  # if inverse is already calculated (not NULL), just recall it
  if (!is.null(invm)) {
    message("getting cached inverse matrix value")
    
    # return the recalled version
    return(invm)
  }
  
  # get the original matrix
  origm <- x$getmat()
  
  # find the inverse of origm with "solve" function
  invm <- solve(origm, ...)
  
  # store the value of inverse (invm) for possible future recall
  x$setinv(invm)
  
  # return the freshly calcualted inverse matrix
  invm
}
