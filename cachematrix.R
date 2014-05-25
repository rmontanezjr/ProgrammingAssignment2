## makeCacheMatrix will create a cache that stores the inverse of 
## the matrix that is stored here.
##
## cacheSolve : calls functions from makeCachematrix.  If the inverse has
## already been solved then makeCacheMatrix will return it.
##
## if not then cacheSolve will calculate the inverse and will save it
## to to the cache.

## makeCacheMatrix 

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<- function(y){
    x<<-y
    m<<-NULL
  }
  get <- function() x
  setinverse<-function(inverse) m<<-inverse
  getinverse<-function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse=getinverse)
}


## Will calculate the inverse of the matrix x if it has not
## been saved in the cache.  If it has been saved in the cache 
## then it will run the solve operation on the matrix.
cacheSolve <- function(x, ...) {
      m<- x$getinverse()
      if(!is.null(m)){
        message("gettting chached data")
        return(m)
      }
      data<-x$get()
      m<-solve(data,...)
      x$setinverse(m)
      m #Return the inverse
        ## Return a matrix that is the inverse of 'x'
}
