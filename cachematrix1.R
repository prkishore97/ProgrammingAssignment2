## makeCacheMatrix function : To cache matrix for inversion
## cacheSolve : To calculate matrix inversion of matrix from from cacheMatrix
## functions do

## A Function to cache Matrix for inversion  

makeCacheMatrix <- function(x = matrix()) {
  inv_mat <- NULL
  set <- function(y) {
    x <<- y
    inv_mat <<- NULL
  }
  get <- function() x
  setinv <- function(solveMatrix) inv_mat <<- solveMatrix
  getinv <- function() inv_mat
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## A Function to calculate matrix inversion, input feed from makeCacheMatrix 

cacheSolve <- function(x, ...) {
       
  inv_mat <- x$getinv()
  if(!is.null(inv_mat)) {
    message("getting cached data")
    return(inv_mat)
  }
  data <- x$get()
  inv_mat <- solve(data)
  x$setinv(inv_mat)
  inv_mat ## Return a matrix that is the inverse of 'x'
}

#r_mat=matrix(c(1,5,2,7,8,6,6,2,5,7,8,9,4,2,5),nrow=3,ncol=3)
#
#cacheSolve(makeCacheMatrix(r_mat))
