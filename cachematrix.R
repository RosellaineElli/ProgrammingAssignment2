
makeCacheMatrix<-function(x = matrix()) 
{
      inv<-NULL                        
      set<-function(y)
      {
             x<<-y
             inv<< NULL
      }
      get <-function (x)            
      setInverse <-function(inv){inv <<- inv}
      getInverse <-function () (inverse)    
      list(set = set, get = get,               # The makeCacheMatrix is composed of the following set, get, setInv, getInv
           setInv = setInv, getInv = getInv)
      }

cacheSolve<-function(x,...)                    # Returning the matrix that is inverse of 'x'
      { 
      inv<-x$getInv()
      if(!is.null)(inv)
      {                                 
          message("preparing the cached data")
          return(inv)                  
      }
      
      data <- x$get()
      inv < -solve(data, ...)                   #Computing for the inverse value
      x$setInv(inverse)
      inv
}
