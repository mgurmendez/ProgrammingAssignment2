# the two functions below implement a way of working with matrices in a way so as to 
# conveniently cache the caluclation of its inverse. 
#
# Example:
# B = matrix( 
# c(2, 4, 3, 1), 
# nrow=2, 
# ncol=2) 
#
#  y <- makeCacheMatrix(B)
# inverse <- cacheSolve(y)
# inverse2 <- cacheSolve(y) # this call will not re-compute inverse, will use cached

# create a cached (wrapped) matrix that allows for caching its inverse

makeCacheMatrix <- function(x = matrix()) {
  # returns a vector with 4 functions (in the following order):
  # set y: sets to y the underlying matrix (needs to comply with the underlying conditions of inversion,
  #                                         this, of course depends on the underlying inverse function)
  # get: gets the underlying matrix
  # setinverse i: stores the underlying inversed matrix 
  # getinverse: gets the stored inverse
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# compute the inverse of a matrix in such a way so as to avoid recomputing the cached matrix

cacheSolve <- function(x, ...) {
  # receives a wrapped matrix x (see makeCacheMatrix on how to create a cached marix object)
  # returns the inverse of the underlying matrix x, using the cached inverse when available
  # the inverse will be cached using R's solve() function, additional arguments to the solve function
  # can be passed as aditional arguments to this function
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
