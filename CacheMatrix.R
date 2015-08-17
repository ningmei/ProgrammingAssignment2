## This function create an object by makeCacheMatrix function,
## If the information exist, 
##         the cacheSolve function can be used to get infomation from the object
## If required infomation not exists, 
##         the cacheSolve funtion will generate that infomation and store in the object.

## The makeCacheMatrix function create an object which store two attributs:
## x - a matrix
## inverse - the inverse of matrix x
## The object have four functions:
## setmatrix - set the value of x
## getmatrix - return matrix x
## setinverse - set the value of inverse
## getinversion - return inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  setmatrix <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  getmatrix <- function() {x}
  setinverse <- function(solve) {inverse <<- solve(x)}
  getinverse <- function() {inverse}
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The cacheSolve function take the object created by makeCacheMatrix
## and return the inverse of the matrix stored in the object.

## This function first check whether the object has the inverse data. 
## If has, function will return the inverse data, 
## if hasn't, function will calculate the inverse of the matrix stored in the object and
## store the result in the object, then return it.



cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$getmatrix()
  inverse <- solve(data)
  x$setinverse(inverse)
  return(inverse)
}
