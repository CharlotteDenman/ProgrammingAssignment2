## Below are two functions that are used to create a special object that stores
## a matrix and cache's its inverse.

## The first function, mackCacheMatrix, creates a matrix, which is really a list
## containing a function to 
##    1. Set the value of the matrix.
##    2. Get the value of the matrix.
##    3. Set the value of the inverse.
##    4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The function below calculates the inverse of the matrix created with the
## above function.
## Note: it first checks to see if the inverse has already been calculated.
## If the inverse has already been caclulated, it gets the inverse from the
## cache and skips the computation.
## If the inverse has not already been calculated, it calculates the inverse
## of the matrix and sets the value of the inverse in the cache via the 
## setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
      message("getting cached data")
      return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
