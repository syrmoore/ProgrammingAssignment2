##This function takes in a matrix and calculates the inverse and stores it in cache
##so you can access it later without recalculating the inverse. If you enter a 
##non-square matrix an error message will be generated.


## This function caches the inverse of the matrix. If you enter a non-square matrix an error messesage will be generated. This function
##allows you to set the inverse of the matrix and get the inverse of the martix.

makeCacheMatrix <- function(x = matrix()) {
  if (nrow(x)==ncol(x)) {
    i <- NULL
    
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  } else {
    print("Error in solve.default(x) matrix must be square")
    }
}


## This funtion returns the inverse of the matrix. After it runs once it will be pulling that matrix from cache so it won't have to
##recalculate the inverse of the matrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
