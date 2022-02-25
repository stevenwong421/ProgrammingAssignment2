## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
      ## return: a list containing functions to
      invmat = NULL
      ##  1. set the matrix
      set = function(y) {
            # use `<<-` to assign a value to an object 
            # different from the current environment. 
            x <<- y
            invmat <<- NULL
      }
      ##  2. get the matrix
      get = function() x
      ##  3. set the inverse
      setinvmat = function(inverse) invmat <<- inverse 
      ##  4. get the inverse
      getinvmat = function() invmat
      list(set=set, get=get, setinvmat=setinvmat, getinvmat=getinvmat)
}

cacheSolve <- function(x, ...) {
      ## x: output of makeCacheMatrix()
      ## return: inverse of the original matrix input to makeCacheMatrix()
      
      invmat = x$getinvmat()
      
      # if the inverse has already been calculated
      if (!is.null(invmat)){
            # get it from the cache and skips the computation. 
            message("getting cached data")
            return(invmat)
      }
      
      # otherwise, calculates the inverse 
      mat.data = x$get()
      invmat = solve(mat.data, ...)
      
      # sets the value of the inverse in the cache via the setinvmat function.
      x$setinvmat(invmat)
      
      return(invmat)
}