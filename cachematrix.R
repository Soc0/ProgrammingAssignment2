
## This function sets the matrix as inputed by the user, sets inverse to NULL
## defines 4 functions used to "set" and "get" matrix and its inverse using "setinverse" and "getinverse"

makeCacheMatrix <- function(x = matrix()) {
  # store the inverse value
  Matrixinverse <- NULL
  # set the original matrix and reset inverse
  set <- function(y) {
    x <<- y
    Matrixinverse <<- NULL
  }
  # get the original matrix
  get <- function() x
  # set inverse value
  setinverse <- function(inverse) Matrixinverse <<- inverse
  # get inverse value
  getinverse <- function() Matrixinverse
  
  # Returns a list of the 4 functions, this list is the special "matrix"
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function Computes, caches, and returns new matrix inverse. 
## It can also return cached matrix inverse using previously computed matrix inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # get inverse value
  Matrixinverse <- x$getinverse()
  # if inverse value not null then return cached data
  if(!is.null(Matrixinverse)) {
    message("getting cached data")
    return(Matrixinverse)
  }
  # else calculate inverse using solve()
  data <- x$get()
  Matrixinverse <- solve(data, ...)
  x$setinverse(Matrixinverse)
  Matrixinverse
}
