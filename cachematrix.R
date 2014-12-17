##function is storing a value or "caching" it in order to save time with computation
##when being recalled

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
  x<<-y 
  m<<-NULL
}
#<<- assigns variable to an environment that is different than the current one
get<-function() x
setmatrix<-function(solve) m<<- solve #getting inverse using solve
getmatrix<-function() m
list(set=set, get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}
#first two functions set up matrix to get values
#reference: http://stackoverflow.com/questions/11995832/inverse-of-matrix-in-r
#reference: http://www.ats.ucla.edu/stat/r/library/matrix_alg.htm

cacheSolve <- function(x=matrix(), ...) {
    m<-x$getmatrix()
    if(!is.null(m)){
      message("Getting cached data!")
      return(m)
    }
    matrix <- x$get() 
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}
#actual cashe function, recalling the value
