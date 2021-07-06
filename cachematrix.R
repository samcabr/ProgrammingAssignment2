#Comment above and at the sides are descriptions or explanations about what the functions do
#These encoded funtions are inorder to accomplish my R:Programming assignment
##The "makeCacheMatrix" allows is to set and get the said value of the matrix or to set the mean value of the matrix
makeCacheMatrix <- function(x = matrix()){          #This functions allows you to get the value
  t <- NULL                                         #In this part I let "t" be the "NULL"
  set <- function(y){
    x <<- y
    t <<- NULL
  }
  get <- function() x    #this is where we will get the matrix's value
  setT <- function(mean) {t <<- mean}               #setting the value of mean
  getT <- function() t                                  #getting the value of mean
  list(set = set, get = get, setT = setT, getT = getT)
}


#Comment above and at the sides are descriptions or explanations about what the functions do
# This function is used to do the cachesolve
cacheSolve <- function(x,  ...){    #If it (matrix) didn't alter then this function will reclaim or retrieve the mean          
  t <- x$getmean()
  if(!is.null(t)){
    message("we are getting cached data...")
    return(t)
  }
  matrx <- x$get()
  t <- solve(matrx, ...)  
  x$setT(t)
  t
}
  
