## The functions described here provide an interface to compute
## the inverse of a matrix and store the result for future
## reference. This minimize the computation time if the matrix 
## is big.

## makeCacheMatrix is a function that creates an object with 
## functionality to store a matrix and its inverse in cache 
## memory.

makeCacheMatrix <- function(x = matrix()) 
{
  inverse <- NULL
  set <- function(y)
  {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve is a function that returns the inverse of a matrix.
## If the inverse have been alredy computed then it returns the 
## stored value. Otherwise, it will compute the inverse, store it, 
## and return its value.

cacheSolve <- function(x, ...)
{
  inverse <- x$getinverse()
  if(!is.null(inverse))
  {
    message("Getting chached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(x$get())
  x$setinverse(inverse)
  inverse
}
