makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrixInv <- function(i) m <<- i
  getmatrixInv <- function() m
  
  list(set = set, get = get,
       setmatrixInv = setmatrixInv,
       getmatrixInv = getmatrixInv) 
  
  
}


cacheSolve <- function(x, ...) {
  m <- x$getmatrixInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  
  
  if (nrow(data)!=ncol(data)) { 
    message("can't be inverted. Input matrix not squred") 
    m<-NULL 
    return(m) 
  }
  
  
  
  if (det(data)==0) { 
    message("can't be inverted. Determinant = 0") 
    m<-NULL 
    return(m) 
  }
  
  
  m <- solve(data)
  x$setmatrixInv(m)
  m
}
