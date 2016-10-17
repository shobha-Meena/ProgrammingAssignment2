## Cache of Matrix Inversion

## Matrix inversion is usually a costly computation. 
## So, instead of calculating it everytime, it is beneficial to cache the inverse 
## The following two functions cache the inverse of a matrix.

## This function creates a special "matrix" object which really is a list 
## contraining a function to  set the matrix, get the matrix, 
## set the inverse of matrix and get the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
     invrs <- NULL
     set <- function(y) {
          x <<- y
          invrs <<- NULL
     }
     get <- function() x
     setInverse <- function(inverse) invrs <<- inverse
     getInverse <- function() invrs
     list(set = set,
          get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated, 
## then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
     invrs <- x$getInverse()
     if (!is.null(invrs)) {
          message("getting cached data")
          return(invrs)
     }
     data <- x$get()
     invrs <- solve(data, ...)
     x$setInverse(invrs)
     invrs
}
