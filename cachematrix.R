## Put comments here that give an overall description of what your
## functions do


## Make and return a makeCacheMatrix object with list of methods to get and 
## set inverse matrices
makeCacheMatrix <- function(x = matrix()) {
  invMat <- NULL
  ## This does the same thing as the initiation of makeCacheMatrix
  ## Reset the value of invMat, and set x equal to y
  ## This is used to reset when solving a new matrix
  set <- function (y){
    x <<- y
    invMat <<- NULL
  }
  
  ## Defined in parent env
  get <- function() x
  
  ## complete calculation to get inverse matrix, then assign to parent variable
  setinv <- function(solve) invMat <<- solve
  getinv <- function() invMat
  
  ## Create a list of type makeCacheMatrix
  ## of methods with names that gets returned to the parent env
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}




## This function solves for the inverse of a given matrix if the inverse 
## is not already stored

cacheSolve <- function(x, ...) {
  invMat <- x$getinv() # retrieve value of invMat via getinv() method
  
  if(!is.null(invMat)){   # See if inverse has been solved, if so, get & return
    message("Getting data from cache!")
    return(invMat)
  }
  data <- x$get() # Otherwise, get x and put into local variable data
  invMat <- solve(data, ...) # use solve to get inverse, and store as invMat
  x$setinv(invMat) # use custom function to set 
  invMat  ## Return a matrix that is the inverse of 'x'
}



# Steps to text
# m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
# 
# myMatrix <- makeCacheMatrix(m1)
# resultMatrix <- cacheSolve(myMatrix)
# print(resultMatrix)