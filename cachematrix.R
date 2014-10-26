## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix function creates a special "matrix" object
## that can cache its inverse. It creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
    ## Initialize the inverse property
  i <- NULL
  
  ## Set the matrix
  set <- function( matrix ) {
          m <<- matrix
          i <<- NULL
  }

  ## Get the matrix
  get <- function() {
      ## Return the matrix
      m
  }
  
  ## Set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Get the inverse of the matrix
  getInverse <- function() {
      ## Return the inverse property
      i
  }
  
  ## Return a list of methods
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

## Computes the inverse of the "matrix" returned
## by makeCacheMatrix(), unless the inverse has
## already been calculated, in which case 
## it retrieves it from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        
        ## Return the inverse if its already set
        if( !is.null(m) ) {
                message("getting cached data")
                return(m)
        }
        
        ## Getting the matrix from the object
        data <- x$get()
        
        ## Calculating the inverse using matrix multiplication
        m <- solve(data) %*% data
        
        ## Setting the inverse to the object
        x$setInverse(m)
        
        ## Returning the matrix
        m
}
