# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) 
{
      inv <- NULL
      setmat <- function(y) 
      {
            x <<- y
            inv <<- NULL
      }
      getmat <- function() x
      setinv <- function(solve) inv <<- solve
      getinv <- function() inv
      list(setmat = setmat, 
           getmat = getmat,
           setinv = setinv,
           getinv = getinv)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...)
{
      inv <- x$getinv()
      if(!is.null(inv)) 
      {
            message("getting cached data")
            return(inv)
      }
      mat <- x$getmat()
      inv <- solve(mat, ...)
      x$setinv(inv)
      inv
}

## Example for using the above functions

## >x <- matrix(c(2, 4, -2, -2), 2, 2)
## >y <- makeCacheMatrix(x)
## >y$getmat()
##     [,1] [,2]
##[1,]    2   -2
##[2,]    4   -2

## No cache in the very first run
## >cacheSolve(y)
##     [,1] [,2]
##[1,] -0.5  0.5
##[2,] -1.0  0.5

## Retrieves the cached data in the second run
## >cacheSolve(y)
##getting cached data
##     [,1] [,2]
##[1,] -0.5  0.5
##[2,] -1.0  0.5
