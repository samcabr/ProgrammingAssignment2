## Put comments here that give an overall description of what your
## functions do

##The "makeCacheMatrix" allows is to set and get the said vale of the matrix or to set the inverse value of the matrix

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}    #this is where we will get the matrix's value
  setInverse <- function(inverse) {inv <<- inverse}               #setting the value of inverse
  getInverse <- function() {inv}                                  #getting the value of inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# This function is used to get the cached data
cacheSolve <- function(x, ...) {
         inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
