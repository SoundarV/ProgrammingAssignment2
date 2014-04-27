## Assignment: Caching the Inverse of a Matrix


## 1: Create Cache of a matrix; 2: Generate inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
                          x <<- y
                          m <<- NULL
                          }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


## Fetch inverse data from cache or recalculate inverse using matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
        m <- x$getinverse()
        if(!is.null(m)) 
          {
            message("getting cached data")
            return(m)
          }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m 
}

##~~~~~~~~~~~~~Output~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
x<-makeCacheMatrix(matrix(c(1,3,2,4),nrow=2,ncol=2)) ##Creating Matrix cache
cacheSolve(x) ## retrieving matrix from cache,calculating and storing inverse matrix to cache and displaying as output
cacheSolve(x)  ## retrieving and displaying stored inverse matrix from cache rather recalculating inverse of matrix
