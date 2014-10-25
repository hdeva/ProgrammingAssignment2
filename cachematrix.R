## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix  has 4 functions
## 1. set : Setting the matrix
## 2. get = getting the matrix
## 3. setinv = setting the inverse matrix
## 4. getinv = getting the inverse matrix
## makeCacheMatrix does not actually do inv - it only returns values
## Built this function based on example provided ie. makeVector

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y) {
    x <<- y
    ## Everytime set is called - we make sure that inv is NULL - Equivalent to dirtying the cache
    inv <<- NULL
  }
  get <- function() x
  setinv <- function ( inverseMatrix = matrix() ) inv <<- inverseMatrix
  getinv <- function() inv
  list (set=set, get=get, setinv = setinv, getinv = getinv )
}


## Write a short comment describing this function
## cacheSolve checks if 'inv' is NULL
## if it is NULL, computes the inv of matix passed to it - Sets the value to inv using x$setinv function
## else returns the cached value in object 'x' by retrieving through 
## function x$getinv

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if ( is.null(inv) ) {
    print("Computing Value for first time")
    inv <- solve(x$get())
    x$setinv(inv)
  }
  else {
    print("Cache Hit")
  }
  inv
}
