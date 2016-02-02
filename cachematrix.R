## Overall description
#   The functions allow you to create the inverse of a matrix and cache the inverse for retrieval
#   Makes use of <<- to assign variables in the parent environment
#   Output of makeCacheMatric can be passed to cacheSolve
#   Example of how to use it:
#     testmatrix <- matrix(c(5,6,7,8),nrow=2,ncol=2)
#     test <- makeCacheMatrix(testmatrix)
#     cacheSolve(test)

## Write a short comment describing this function
#   makeCacheMatrix takes a matrix as input - assuming square and invertible for this exercise
#   initializes the inverse to NULL
#   returns a list of 4 functions set, get, setinverse, getinverse
#   get and getinverse simply return the original matrix and i (=NULL or the calculated inverse)
#   set and setinverse assign the matrix and the inverse

makeCacheMatrix <- function(x=matrix()){
  i <- NULL
  set <- function(y) {
    x <<- y 
    i <<- NULL 
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
#   takes the list from makeCacheMatrix as input
#   if the inverse been calculated already (ie cacheSolve has been run on the matrix already)
#   then the cached inverse is returned.  If this is the first time cacheSolve has been run, the matrix
#   is retrived from the list as x$get(), the inverse obtained using solve and stored in i and cached in
#   x$setinverse(i)

cacheSolve <- function(x, ...){
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
