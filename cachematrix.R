## The functions cache the inverse of a matrix in order 
## to avoid repetition of time-consuming computations

## This function creates a special "matrix" object that caches the inverse 

makeCacheMatrix <- function(x = matrix()) {
s<-NULL
  set<-function(y){
    x<<-y
    s<<-NULL
  }
  get<-function() x 
  setinverse<-function(minverse) s<<-minverse
  getinverse<-function() s
  list(set=set, get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function computes the inverse of the "matrix" 
## (if the inverse has been already calculated, it retrieves from the cache)

cacheSolve <- function(x, ...) {
  s<-x$getinverse()
  if (!is.null(s)){
    message("getting cached data")
    return(s)
  }
  data<-x$get()
  s<-solve(data,...)
  x$setinverse(s)
  s      
  ## The function returns a matrix that is the inverse of the "matrix" 'x'
}
