## makeCacheMatrix creates the Matrix function
## get functions gets the value of the matrix
## setinv function sets the inverse of the matrix
## set function sets the value of the matrix
##getinv function gets the inverse of the matrix
## cacheSolve function gets the cached inverse of the matrix

makeCacheMatrix <- function(x = matix()) {
  row<-nrow(x)
  col<-ncol(x)
  m <- matrix(data = NA, nrow=row,ncol=col)
  set <- function(y) {
    x <<- y
    m <<- matrix(data = NA, nrow=row,ncol=col)
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!all(is.na(m))) {
    message("getting cached inv")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}