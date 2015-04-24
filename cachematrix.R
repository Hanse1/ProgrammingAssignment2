## makeCacheMatrix is a function that stores a list of additional functions
## specifically: set, get, setinverse, getinverse for a matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## set will change the matrix that was originally stored
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## get just pulls the matrix you created
  get <- function() x
  
  ## setinverse and getinverse do not calculate the inverse; rather they are used to
  ## store the value of the input in a variable m, similar to set and get
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve is a function that checks to see if the inverse has been previously
## stored with getinverse, otherwise it solves for the inverse of the matrix

cacheSolve <- function(x, ...) {

  ## function checks to see if getinverse already has a value stored and is not null
  ## if value was previously stored, it will return it
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## if getinverse does not have a value (ie. Null), then it will get the matrix and
  ## use the solve function which returns the inverse
  ## finally it stores the inverse to the setinverse function and prints the results
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
