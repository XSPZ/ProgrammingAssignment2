makeCacheMatrix <- function(x = matrix()) {
          m <- NULL
          set <- function(y) {
                x <<- y
                m <<- NULL
          }                 ## set the function to get the matrix
          get <- function() x   ## get the matrix
          setinverse <- function(ginv) m <<- ginv   ##set the inverse of the matrix
          getinverse <- function() m            ##get the inverse of the matrix
          list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
   
  
}


cacheSolve <- function(x, ...) {
  library(MASS) ## MASS package is to calculate square and non-square 
                ## matrix's inverse
  m <- x$getinverse()   ## get the inverse of the matrix
  if(!is.null(m)) {
          message("getting cache data")
          return(m)
  }
  data <- x$get()   ## get the matrix
  m <- ginv(data, ...)    ## calculate the inverse of the matrix
  x$setinverse(m)
  m
  
}
