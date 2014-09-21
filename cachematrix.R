## This function creates a special "matrix" object that can cache its inverse.
## The first function, makeCacheMatrix creates a list containing a function to
## 1. set the value of the "matrix" object
## 2. get the value of the "matrix" object
## 3. set the value of the inverse of the "matrix" object
## 4. get the value of the inverse of the "matrix" object

makeCacheMatrix <- function(x = matrix()) {
  inv  <- NULL
  set  <- function(y){
    x <<- y
    inv <<- NULL 
  }
  get  <- function() x
  setinverse  <- function(inverse) inv  <<- inverse
  getinverse  <- function() inv
  list(set= set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
  
} ## end of function makeCacheMatrix()


## This function computes the inverse of a invertible matrix. If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache otherwise computes the inverse again

cacheSolve <- function(x, ...) {
  inv  <- x$getinverse()
  if (!is.null(inv)){
    message("getting inverse of matrix from cache...")
    return(inv)
  }
  data  <- x$get()
  inv  <- solve(data, ...)
  x$setinverse(inv)
  inv
} ## end of function cacheSolve()
