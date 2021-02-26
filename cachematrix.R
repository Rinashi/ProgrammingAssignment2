## Put comments here that give an overall description of what your
## functions do

##There are 2 functions makeCacheMatrix and cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  p <- NULL
  set <- function(y){
    x <<- y
    p <<- NULL    #initializing inverse as NULL
  }
  get <- function()x  #to get matrix x
  setInverse <- function(inverse) p <<- inverse
  getInverse <- function() p 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Write a short comment describing this function
## To get cache data

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  p <- x$getInverse()
  if(!is.null(p)){    #To check if inverse is NULL
    message("getting cached data")
    return(p)          #To get inverse data 
  }
  mat <- x$get()
  p <- solve(mat,...)   # To calculate inverse data 
  x$setInverse(p)
  p ## Return a matrix that is the inverse of 'x'
}
