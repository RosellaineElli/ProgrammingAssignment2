## A two functions can be used makeCacheMatrix, makeCacheMatrix
## The makeCacheMatrix is composed of set, get, setInv, getInv

makeCacheMatrix <- function(x = matrix()) 
{
      inv<-NULL                        # Preparing inverse as NULL
      set<-function(y)
      {
             x<<-y
             inv<< NULL
      }
      get<-function (x)            #Function used to get matrix x
      setInverse<-function(inv){inv <<- inv}
      getInverse<-function () {inv}    
      list( set = set,
           get = get, 
           setInverse = 
           setInverse, 
           getInverse = getInverse)
      }

## Using this we can get the cache data
cacheSolve<-function(x,...)          ##Getting cache data
      { 
      inv<-x$getInverse()
      if(!is.null)(inv)
      {                                 #Checking if inverse is Null
          message("preparing cached data")
          return(inv)                   #Returns the inverse value
      }
      mat<-x$get()
      inv<-solve(mat,...)              #Computing inverse value
      x$setInverse(inv)
      inv
        ## Returning the matrix that is inverse of 'x'
}
