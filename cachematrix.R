## The functions cache information that can be returned later

## makeCacheMatrix() takes a vector and turns it into a square matrix. If it's already a square matrix,
## then it stays the same. It stores the matrix in the function get(), and the default inverse of NULL in
## the function getinverse(). Both of these functions simply return the stored value. When setinverse()
## is called, it updates the value of the inverse i. These custom functions are stored in a list, from which
## they can be called.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  x <- matrix(x, sqrt(length(x)), sqrt(length(x)))
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve() requires from the list output from makeCacheMatrix(). Using the custom functions there, it
## assigns i a value using the getinverse() in the list from makeCacheMatrix(). If the value of i is not
## NULL then a message is returned saying "getting cached data" followed by the value of i, and the program is
## stopped. If i does equal NULL, then matrix is retrieved using get() and then the inverse is calculated
## using the built-in function solve(). cacheSolve() then calls the custom function setinverse() and updates the
## cached value for the inverse before returning it and ending the program.

cacheSolve <- function(CacheMatrix, ...) {
  i <- CacheMatrix$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    i
  }
  i <- solve(CacheMatrix$get())
  CacheMatrix$setinverse(i)
  i
}
