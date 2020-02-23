# (1) The first function creates a matrix that can cache its inverse.
#     This function is called makeCacheMatrix.

# (2) The second function computes the inverse of the special matrix 
#     created by makeCacheMatrix. If the inverse of the matrix exists
#     already (and the matrix has not changed), then the function 
#     cacheSolve should simply retrieve the inversefrom the cache.



# The function makeCacheMatrix() creates a matrix whose inverse 
# can be cached. It does this by first creating an empty matrix
# (with dimensions equal to the input matrix) that will be populated 
# by a function that computes the inverse of the input matrix.
# It is worth noting that the input matrix must always be square,
# non-singular and A*A' = A'*A = Identity Matrix (with 
# dimensions equal to the input matrix), where A' is the inverse
# of A. This function contains 4 other functions, where the first
# sets a newly entered matrix to the default matrix 'x', the second
# retrieves the matrix 'x', the third assigns the inverse of 'x' 
# to m_inverse and the fourth retrieves that inverse from the 
# parent environment. This function leverages the lexical scoping
# rules of R to access all these functions and objects that are
# defined in their respective environments. The output of this
# function is a list of four functions to be passed to cacheSolve().
 

makeCacheMatrix <- function(x = matrix()) {
  m_inverse <- matrix(data=NA, nrow=dim(x)[1],ncol=dim(x)[2])
  set <- function(new_matrix) {
    x <<- new_matrix
    m_inverse <<- matrix(data=NA, nrow=dim(x)[1],ncol=dim(x)[2])
  }
  get <- function() x
  setinv <- function(inverse) m_inverse <<- inverse
  getinv <- function() m_inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


# The function cacheSolve() takes in the output of makeCacheMatrix and 
# retrieves an object called m_inverse which is the inverse of 
# the input matrix by calling the getinv() function. If the 
# retrieved inverse is not empty, it will print a message and
# return the inverse. If it doesn't exist, it will use the built-in
# solve function to compute the inverse, check if it satisfies the
# condition A%*%A' = A'%*%A = Identity Matrix (of length equal to A)
# and if it's TRUE, returns the inverse called m_inverse.

cacheSolve <- function(x, ...) {
  m_inverse <- x$getinv()
  if(all(!is.na(m_inverse))){
    message("getting cached data")
    return(m_inverse)
  }
  
  data <- x$get()
  m_inverse <- solve(data,...)
  if(data%*%m_inverse == diag(nrow=dim(data)[1]) && m_inverse%*%data == diag(nrow=dim(data)[1])){
          #print("m_inverse is the correct inverse")
          m_inverse <- m_inverse
    }
  m_inverse
}