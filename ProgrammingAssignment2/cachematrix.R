## 

## This function keeps the x (matrix) and m (reverse matrix of x) and returns the values on demand

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  ## sets the local value x to passed value of y and also sets the m to null, as x has changed
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  
  ## Get the matrix x
  get<-function() x
  ## set the inverse matrix
  setmatrix<-function(solve) m<<- solve
  ## returns the matrix m
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
      
}
