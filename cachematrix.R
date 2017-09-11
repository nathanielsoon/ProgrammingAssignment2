## The functions below create a special 'matrix' object that
## can be cached and solves the inverse of the matrix 
## if it has not been cached or retrieves the inverse of 
## the matrix if it has been cached. Functions only work on
## square invertible matrices.

## makeCacheMatrix creates a list of functions to:
## 1. Set a matrix ($set)
## 2. Get/retrieve a matrix ($get)
## 3. Set the inverse of the matrix ($setinv)
## 4. Get/retrieve the inverse of a matrix ($getinv)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve retrieves the inverse of the matrix if 
## it is cached or gets the inverse of the matrix if
## it is new or not cached yet

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
