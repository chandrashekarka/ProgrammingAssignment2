makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrixinverse<-function(solve) m<<- solve
  getmatrixinverse<-function() m
  list(set=set, get=get,
       setmatinv=setmatrixinverse,
       getmatinv=getmatrixinverse)
}

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatinv(m)
  m
}
