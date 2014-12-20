## This pair of functions attempt to cache the inverse of a matrix

## As already stipulated,this function creates a 
## special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  InverseOfX <- NULL 
  set <- function(y) {
    x <<- y
    InverseOfX <<- NULL 
  }
  
  get <- function() x 
  setInv <- function(inv) InverseOfX <<- inv 
  getInv <- function() InverseOfX 
 
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()  
  if(!is.null(m)) { 
    message("getting cached data")
    return(m) 
  }
  data <- x$get()
  m <- solve(data) 
  x$setInv(m) 
  m 
  
}
