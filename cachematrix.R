## This is the solution by Bhargavi Panchangam for second programming assignment in R Programming course
## This R script contains two functions stated below, which are used to cache a matrix and calculate its inverse
## We are using scoping rules of R language in this exercise
## Typically calculating inverse of a matrix is a computationally intensive and costly method, especially if it has to be computed in a loop. If contents are not changing, it makes sense to cache the inverse of a matrix and re-use it when needed, when it can be looked up in the cache instead of computing again.

## The following function creates a special "matrix" object that can cache its inverse, which is really a mtrix containing function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  set <- function(y){
    x <<- y
    invrs <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invrs <<- inverse
  getinverse <- function() invrs
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The following function computes the inverse of the special "matrix" returned by the above function. If the inverse has already been calculated (and the matrix has not changed), then this function should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  invrs <- x$getinvers()
  if(!is.null(invrs)){
    message("getting cached inverse data")
    return(invrs)
  }
  data <- x$get()

  ## Return a matrix that is the inverse of 'x'
  invrs <- solve(data)
  x$setinverse(invrs)
  invrs
}
