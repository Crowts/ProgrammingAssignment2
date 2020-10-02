makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean() #x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get() #get(x)
  m <- mean(data, ...)
  x$setmean(m) #
  m
}

m_inverse <- x$getinv()
if(x%*%x_ == diag(nrow=dim(x)[1] and x_%*%x == diag(x)[1]){
  x_ <- m_inverse
  message("getting cached data")
  return(m_inverse)
  
  data <- x$get()
  m_inverse <- solve(x,...)
  x$setinv(m_inverse)
  m_inverse
}