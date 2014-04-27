## Overall descriptions of the two functions
## =========================================

## We have two functions makeCacheMatrix() and cacheSolve().

## The function makeCacheMatrix() takes as input an invertible squared matrix
## and returns an list that contains four functions and their corresponding environment.
## The matrix has been provided as input is stored into the environment of the
## functions.

## The function cacheSolve() takes as input an object that has been produced
## by the previous function makeCacheMatrix(). It uses the four functions in the 
## object and the matrix saved in the environmen to make the following use.
## If the inverse matrix had been stored in the environment previously, then 
## the function returns automatically that stored inverse matrix.
## If the inverse matrix had not been stored previously, then it calculates the 
## inverse matrix, stores the inverse matrix in the environement of the object (so
## that it is in the memory cache) and returns the inverse matrix.


## Description of the matrix makeCacheMatrix()
## ===========================================

## Input: 'x' is an invertable square matrix to be inverted. 

## Returns a list with four functions and the corresponding environment,
## where the matrix is kept stored in the cash.
## 
## The four functions are:
## set:         This function replaces the matrix that has been provided and deletes 
##              a potential previously calculated inverse matrix. (This specific
##              function is not used in this assignment, but is usefull
##              for further use of the code.)
## get:         This function returns the matrix that has been provided as input.
## setinverse:  This function stores the inverse of the matrix in the environment in 
##              the cache. 
## getinverse:  This function returns the inverse matrix that had been stored in 
##              the cache.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(solve) inv <<- solve
  
  getinverse <- function() inv

  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

## Description of the matrix cacheSolve()
## ======================================

## The input 'x' is an object that has been created with the function 
## makeCacheMatrix().

## It contains a list with four functions and a corresponding environment in the
## cache, where the original squared matrix is stored and eventually also the
## inverse of the matrix if it had been already been calculated previously.

## The function checks if the inverse had already been calculated and 
## stored in the environment by making use of the function getinverse() in 
## the list.

## If the inverse exists, this inverse matrix is returned after an additional 
## message informing about it.

## If the inverse had not been stored in environment, then the original squared
## matrix is accessed with the function get() in the list.
## With the squared matrix at hand the inverse is calculated, and is then
## stored in the enviroment by the list function setinverse(). Finally the
## calculated inverse matrix is returned.

cacheSolve <- function(x, ...) {

  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  
  inv
}
