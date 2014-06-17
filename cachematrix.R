## Implements a function that allows storage of a matrix
## and provides an interface to allow for caching of data
## and the inverse matrix generated
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    ##set signature
    set <- function(y) {
      x <<- y
      inverse <<- NULL
    }
    
    ##get signature
    get <- function() x
    
    setinverse <- function(soln) inverse <<- soln
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}

## A more robust implementation of the above
## that removes the need for the 'cacheSolve' function
## and defines the inverse directly with the setinverse
## method. The getinverse method now looks for a cached
## inverse matrix, or calls setinverse and caches the
## result
cMatrixWithInverse <- function(x = matrix()) {
  inverse <- NULL
 
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  
  ##solve for the inverse if setinverse is called
  setinverse <- function() inverse <<- solve(x)
  
  ##check for cached inverse, or setinverse directly
  getinverse <- function(){
    ##check the cache
    if(!is.null(inverse)) {
      message("getinverse: getting cached data")
      return(inverse)
    }
    ##no cache, create inverse and return it
    setinverse()
    inverse
  }
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse
      )
}