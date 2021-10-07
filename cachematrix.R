## Put comments here that give an overall description of what your
## functions do

## A two functions can be used makeCacheMatrix, makeCacheMatrix
## The makeCacheMatrix is composed of set, get, setInverse, getInverse
## A library(MASS) can be used to compute both inverse for the non squared matrices and squared matrices

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL                        # Preparing inverse as NULL
      set <- function(y){
             x <<- y
             inv <<- NULL
      }
      get <- function() {x}              #Function used to get matrix x
      setInverse <- function(inverse) {inv <<- inverse}
      getInverse <- function () {inv}    
      list( set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## Using this we can get the cache data

cacheSolve <- function(x, ...)          ##Getting cache data
      { 
      inv <- x$getInverse()
      if(!is.null)(inv){                #Checking if inverse is Null
          message ("getting cached data")
          return(inv)                   #Returns the inverse value
      }
      mat <- x$get()
      inv <- solve(mat, ...)              #Computing inverse value
      x$setInverse(inv)
      inv
        ## Return a matrix that is the inverse of 'x'
}
