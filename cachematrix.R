## This R script contains two functions that will successfully cache the inverse of the matrix, rather than computing it repeatedly
##Also, this assignments assume all matrices will be invertible and square.

## The function, makeCacheMatrix, will create the special matrix that can cache its inverse. It will first
## set the matrix, get the value of the matrix, set the value of the inverse, and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
    }
  get<-function()x
  setInverse<-function(){ i<<-solve(x) }
  getInverse<-function() i
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  }


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix funciton above.
## If the inverse has already been calculated, then cacheSolve function should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  i<- x$getInverse()
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
    }
  data <- x$get()
  i<-solve(data,...)
  x$setInverse()
  i
}
