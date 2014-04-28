## cacheMatrix.R provides functions to create an object which provides
## functions to access a cached matrix and the matrix's inverse.

## creates the special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
     inverse <- NULL
     set <- function(y) {
          x <<- y
          inverse <<- NULL ## reset cache
     }
     get <- function() x
     setinverse <- function(i) inverse <<- i
     getinverse <- function() inverse
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse) ## create matrix object as a list
}


## returns the inverse a cacheMatrix computing the inverse only if
## not already cached, or the cacheMatrix has been updated

cacheSolve <- function(x, ...) {     
     inverse <- x$getinverse()
     if(!is.null(inverse)){
          return(inverse) ## inverse has already been calculated
     }
     inverse <- solve(x$get())
     x$setinverse(inverse) ## cache inverse
     
     inverse
}
