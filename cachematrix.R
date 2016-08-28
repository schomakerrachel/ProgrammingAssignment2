
#makeCacheMatrix creates a function that takes a matrix and receives the inverse
#of the matrix as the output

#CacheSolve makes a function that takes the output from makeCacheMatrix and gives the
#inverse of the original makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) { #makes function makeCacheMatrix
  inv = NULL
  
  set = function(y) {
    x <<- y #use <<- to assign value in env different from current env (see instructions)
    inv <<- NULL
  }
  
  get = function() x #gets the matrix
  setinv = function(inverse) inv <<- inverse #sets the inverse
  getinv = function() inv #retreives the inverse 
  list(set=set, get=get, setinv=setinv, getinv=getinv) #gets the inverse
}

cacheSolve <- function(x, ...) { #makes function cacheSolve
  inv = x$getinv()
  
  if (!is.null(inv)){    #if inverse calculated, get it from cache
    message("getting cached data")
    
    return(inv) #returns inverse
  }

  mat.data = x$get() #calculates inverse if not already done
  inv = solve(mat.data, ...)
  
  x$setinv(inv) #sets inverse value in cache
  
  return(inv) #returns inverse
}