## These functions calculate the inverse of a matrix and then store that inverse in memory.
## If the inverse of the same matrix is called again, any number of times, it will be recalled
## from memory, rather than be recalculated.
## A new matrix entered into the function will result in the inverse stored in memory being set to NULL 
## and a new inverse matrix being stored in memory, which can now be recalled if needed.

## This function contains the matrix object, x, the inverse matrix, i, and 4 other functions that are
## needed as arguments in the next function (cacheSolve)

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function returns the inverse, i, of matrix x. If a value for i is stored in memory, it
## will return that value. Otherwise it will calculate it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}

