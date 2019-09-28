## Creates a special matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  i<- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  
  setInverse <- function(inverse)  i <<- inverse
  getInverse <- function() i
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix" or 
## retrieve it from the cache if it has been previously calculated.

cacheSolve <- function(x, ...) {
  ##cached inverse
  i <- x$getInverse()
  if( !is.null(i) ) {
    message("getting cached data")
    return(i)
  }
  
  ## calculated inverse
  data <- x$get()
  i <- solve(data) %*% data
  x$setInverse(i)
  i
}
