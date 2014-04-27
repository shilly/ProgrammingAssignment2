## This file contains two functions 1)makeCacheMatrix and 2)  cacheSolve()
## makeCacheMatrix() - Takes a matrix as input and returns a list of functions
## that can be performed on the given matrix.
## cacheSolve()- This function takes "Special" matrix created by makecacheMatrix()
## and get the inverse of thematrix and caches the inverse.So all the subsequent
## calls of cacheSolve() will return the cached output.


## makeCacheMatrix() - Takes a matrix as input and returns a list of functions like
## -set,get,setInverse,getInverse ,that can be performed on the given matrix.
## These functions can be called explicitly on the given matrix 
## or can be called from other functions.

makeCacheMatrix <- function(x = matrix())
{
  
  invmat <- NULL
  set <- function(y) 
  {
    x <<- y
    invmat <<- NULL
  }
  get <- function() x
  setInverse <- function(matx) invmat <<- matx
  getInverse <- function() invmat
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  

}


## cacheSolve()- This function takes "Special" matrix created by makecacheMatrix()
## and get the inverse of thematrix and caches the inverse.So all the subsequent
## calls of cacheSolve() will return the cached output.

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
  
  invmat <- x$getInverse()
  if(!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }
  data <- x$get()
  invmat<- solve(data, ...)
  x$setInverse(invmat)
  invmat
  
}
