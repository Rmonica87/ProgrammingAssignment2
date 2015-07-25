## makeCacheMatrix(): creates a special "matrix" object that can cache its inverse.
##cacheSolve(): computes the inverse of the "matrix" returned by makeCacheMatrix().
## @x: a square invertible matrix

makeCacheMatrix <- function(x = matrix()) {

## return: a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
##         this list is used as the input to cacheSolve()

inv = NULL
set = function(y) {
  x <<- y
  inv <<- NULL
}
get = function() x
setinv = function(inverse) inv <<- inverse 
getinv = function() inv
list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv = x$getinv()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat.data = x$get()
  inv = solve(mat.data, ...)
  x$setinv(inv)
  
  return(inv)
  
  }
