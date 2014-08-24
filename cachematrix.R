## Functiom Name: makeCacheMatrix 
## Parmeters: accepts a matrix argument
## This function creates a special "matrix" object that can cache its inverse.
## It works if the matrix is square

makeCacheMatrix <- function(x = matrix()) {
  ## Intialize the matrix
     i_matrix <- NULL
     set <- function(y) {
       x <<- y
       i_matrix <<- NULL
    }

  ## Retrieve the values of the matrix
      get <- function() x
  
  ## invert the matrix using solve (only if it is a square matrix)
      setinv <- function(solve) i_matrix <<- solve
      getinv <- function () i_matrix
  
  ## retrieve the inverted matrix
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Functiom Name: cacheSolve
## Parmeters: accepts what is returned from makeCacheMatrix(x) shown above
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix(x) above. 
## It checks to see if the inverse has already been calculated (the matrix has not changed), 
## If it has been caclulated, the then cacheSolve retrieves the inverse from the cache and
## that it is doing so in the log.

cacheSolve <- function(x, ...) {
  ## retrieves the inverse of the matrix
  i_matrix <- x$getinv()
  ## if i_matrix is not null then it must already be have calulated.  
  ## Let the user know it is retrieving the cached version.
  if(!is.null(i_matrix)) {
    message("getting cached data")
    return(i_matrix)
  }
  ## If the matrix was null, calculate the inverse and store it in 
  ## the cache
  storedata <- x$get()
  i_matrix <- solve(storedata, ...)
  x$setinv(i_matrix)
  ## display the matrix
  i_matrix
}
