## Put comments here that give an overall description of what your
## functions do

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  mat_inv <- NULL
  set_mat <- function(y) {
              x <<- y
              mat_inv <- NULL
  }
  get_mat <- function() x
  set_inv <- function(mi) mat_inv <<- mi
  get_inv <- function() mat_inv
  list(set = set_mat, get = get_mat, set_inv = set_inv, get_inv = get_inv)
}


## This function retrievs/computes the inverse of the special "matrix" returned by makeCacheMatrix based on weather the inverse has already been calculated.

cacheSolve <- function(x, ...) {
        mat_inv <- x$get_inv()
        if(!is.null(mat_inv))
        {
          message("The inverse of the matrix is...")
          return(mat_inv)
        }
        mat <- x$get()
        mat_inv <- solve(mat)
        x$set_inv(mat_inv)
        mat_inv
}
