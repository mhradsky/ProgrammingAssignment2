## These functions caches the inverse of a matrix initially set at makeCacheMatrix

## This Function stores 4 Sub Functions.
## The set Function changes the matrix stored in the main function
## The get Function gets the matrix stored in the set function
## The setinvers and getinvers Function store the value of the input in a variable s into
## the main function (setinvers) and return it (getinvers)

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y){
    x<<-y
    s<<- NULL
  }
  get <- function() x
  setinvers <- function(solve) s <<- solve
  getinvers <-function() s
  list (set=set, get=get, setinvers=setinvers, getinvers=getinvers)
}


## Input of cacheSolve is the object where makeCacheMatrix is stored.
## It first looks wheather an inverse matrix is cached and if this is not the case it generates the inverse
## of the matrix and stores it

cacheSolve <- function(x, ...) {
  s<-x$getinvers()
  if (!is.null(s)) {
    message("getting cached data")
    return (s)
  }
  data <-x$get()
  s<- solve(data, ...)
  x$setinvers(s)
  s
}

