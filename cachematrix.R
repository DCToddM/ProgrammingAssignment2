## makeCacheMatrix does these 4 specific things
## 1.  Sets the value of the matrix
## 2.  Gets the value of the matrix
## 3.  Sets the inverse of the matrix
## 4.  Gets the inverse of a matrix.
##
## The cacheSolve function caluclates the value of the inverse of the matrix
## if that value does not already exists in Cache.

## This function creates a matrix and then saves the matrix into cache.

makeCacheMatrix <- function(x = matrix()) {
     
     inv <- NULL
     
     set <- function(y){
          x <<- y
          inv <<- NULL
     }
     
     get <- function() x
     setinv <- function(inverse) inv <<- inverse
     getinv <- function() inv
     
     list(set = set, get = get, setinv = setinv, getinv = getinv)
     
}


## This function will check to see if inverse of matrix already exists in cache.
## If inverse does not exist this function will calculate inverse of matrix.  If
## inverse exists and matrix has not changed this function will retrieve inverse
## from chache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'inv'
          inv <- x$getinv()
          
          if(!is.null(inv)) {
               message("getting cached data")
               return(inv)
          }
          
          data <- x$get()
          
          inv <- solve(data)
          
          x$setinv(inv)
          
          inv
}
