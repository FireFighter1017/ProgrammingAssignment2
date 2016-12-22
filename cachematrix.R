## Programming aSsignment no. 2from R Programming Class at Coursera.org
## Lexical Scoping

## The functions coded here are caching an inversed matrix
## so it does not have to be computed again.
## This is usefull when the data does not change
## So we can use the functions to store or retrieve the inverse matrix

##************************************************************
##  -- MAKECACHEMATRIX --
## -----------------------------------------------------------
## The first function exposes a list of functions
##    setsrc: Stores the source matrix using "set" function
##    getsrc: Returns the source matrix using "get" function
##    setinverse: inverse matrix and caches it using "setimx" function
##    getinverse: Returns inverse matrix from cache using "getimx" function

makeCacheMatrix <- function(x = matrix()) {
  ## At first run, reset cached invers matrix
    imxCached <- NULL
  
  ## -- SET --
  ## Stores matrix  
  setsrc <- function(y) {
    x <<- y
    imxCached <<- NULL
  }

  ## -- GET
  ## Returns source matrix
  getsrc <- function() x
  
  ## -- SETIMX
  ## Stores in cache inverted matrix
  setinv <- function(inverted) imxCached <<- inverted
  
  ## -- GETIMX
  ## Retrieve inverted matrix from cache
  getinv <- function() imxCached
  
  ## -- Exposed list of functions
  list(setsrc=setsrc, getsrc=getsrc, setinv=setinv, getinv=getinv)
}

##************************************************************
##  -- CACHESOLVE --
## -----------------------------------------------------------
## The following function returns the inverse of the matrix. 
## This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  

  imx <- x$getinv()
  if(is.null(imx)) {
    ## Retrieve source matrix
    srcmx <- x$getsrc()
    ## Inverse matrix
    imx <- solve(srcmx)
    ## send it to cache
    x$setinv(imx)
  }
  else {
  ## If inverted matrix already exists
  ## inversion is skipped and return cached inv. matrix
    message("getting cached data.")
    imx <- x$getinv
  }
  
  ## return inversed matrix
  imx
}
