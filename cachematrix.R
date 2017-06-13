## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  c <- NULL
  set <- function(y) {
    x <<- y
    c <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) c <<- inverse
  getinverse <- function() c
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  c <- x$getinverse()
  if(!is.null(c)){
    message("Retrieving inverse of matrix from cache...")
    return(c)
  }
  data <- x$get()
  c <- solve(data)
  x$setinverse(c)
  c
}
