## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates a special "matrix" object that can cache its inverse
## 
makeCacheMatrix <- function(x = matrix()) {
  inv_x <- matrix();
  set <- function(y) {
    x <<- y
    inv_x <<- matrix()
  }
  get <- function() x
  set_inv <- function(mat) inv_x <<- mat
  get_inv <- function() inv_x
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
  
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.  
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_x <- x$get_inv()
  if(!is.na(x)) {
    message("getting cached data")
    return(inv_x)
  }
  mat <- x$get()
  inv_mat <-solve(mat)
  x$set_inv(inv_mat)
  inv_mat
  
  
}
