## Assignment: Caching the Inverse of a Matrix


## Fuction 1: makeCacheMatrix; Function to accept matrix value as input and Create Cache of a matrix;

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
                          x <<- y     ##assigning matrix to x globally
                          m <<- NULL  ##assigning null to m globally
                          }
      get <- function() x             ##function to get the stored matrix
      setinverse <- function(inverse) m <<- inverse   ##function to set the inverse matrix
      getinverse <- function() m      ##function to get inverse matrix
      list(set = set, get = get,      
            setinverse = setinverse,
            getinverse = getinverse)  ##listing the stored value of set,get,setinverse and getinverse
}


## Fetch inverse data from cache or calculate inverse using matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
        m <- x$getinverse()               ##retrieve inverse value from cache
        if(!is.null(m))                   ##validate if inverse matrix is in cache. If yes return that as output.
          {
            message("getting cached data")
            return(m)
          }
        data <- x$get()                 ## if no then get matrix from cache and calculate inverse matrix
        m <- solve(data, ...)
        x$setinverse(m)                 ## store calcualted inverse matrix in cache
        m 
}

##~~~~~~~~~~~~~Output~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
x<-makeCacheMatrix(matrix(c(1,3,2,4),nrow=2,ncol=2)) ##Creating Matrix cache
cacheSolve(x) ## retrieving matrix from cache,calculating and storing inverse matrix to cache and displaying as output
cacheSolve(x)  ## retrieving and displaying stored inverse matrix from cache rather recalculating inverse of matrix
