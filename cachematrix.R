
## makeCacheMatrix that will create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  set<- function(y){
    x<<-y
    m<<- NULL
  }
  get<-function()x
  setInverse<- function(solveMatrix) m<<-solveMatrix
  getInverse<-function() m
  list(set=set, get=get, setInverse=setInverse,getInverse=getInverse)
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  m<-x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get
  m<-solve(data)
  x$setInverse(m)
  m
}



