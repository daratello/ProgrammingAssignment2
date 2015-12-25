## cachematrix.R
##
## Create a cache marix object that can be used to
## repeatably solve the inverse of the marix, but only
## calculates the inverse once.
##   makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##   cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), then the cache solve should
 ## retrieve the inverse from the cache.

##
## Usage:
##  M <- matrix(c(1, 2, 3, 4), nrow=2, ncol=2)
##  cacheMatrix <- makeCacheMatrix(M)
##  cacheSolve(cacheMatrix)
##
##  cacheMatrix$set(M)      # Change the matrix being cached.
##  M <- cacheMatrix$get()  # Returns the matrix being cached.
##
##  cacheMatrix$setInverse(solve(data, ...)) # Private function containing cached inverse of x
##  cacheMatrix$getInverse()                 # Private function used to get the cached inverse of x

## Create a cacheMatrix object for an invertale matrix.

makeCacheMatrix <- function(x = matrix()) {
       cachedInverse <- NULL
  set <- function(y) {
      x <<- y
              cachedInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) cachedInverse <<- inverse
   getInverse <- function() cachedInverse
           list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Return the inverse of an cacheMatrix object

cacheSolve <- function(x, ...) {
  
  
  ## Return a matrix that is the inverse of 'x'
   invFunc <- x$getInverse()
  if(!is.null(invFunc)) {
    message("getting cached data")
         return(invFunc)
  }
  data <- x$get()
  invFunc <- solve(data, ...)
      x$setInverse(invFunc)
      invFunc
}
