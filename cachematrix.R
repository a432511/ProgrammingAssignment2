## cachematrix will return an object capable of maintaining a cached version
## of the inverse of a given matrix thereby saving valuable computation time

## makeCacheMatrix accepts an optional matrix as a parameter and returns
## an object with several helper functions. 
## `get` will return the original matrix
## `set` will override the original matrix and reset the cached value
## `setinverse` sets the inverse of the matrix and writes it to cache
## `getinverse` retrieves the cached inverse matrix

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


## cacheSolve is a wrapper for the `solve` function that expects the pseudo-matrix
## that was constructed using `makeCacheMatrix`
## cacheSolve will return the cached inverted matrix if it exists, otherwise it
## will compute the inverse of the matrix, cache it, and return the inverse matrix
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
