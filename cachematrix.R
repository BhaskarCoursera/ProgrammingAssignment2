## The first function instantiates the list of 4 functions.
## The second function calculates and sets the inverse in case 
## it has not been calculated before

## Creates the initial list object containing the functions

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


## Calculates Inverse and sets it if it has not been calculated before

cacheSolve <- function(z, ...) {
        ## Return a matrix that is the inverse of 'x'
    m1 <- z$getinverse()
    
    
    if (length(m1) != 0) {
      message("getting cached data")
      return(m1)
      }
         
    message("Calculating Inverse")
    data <- z$get()
    m1 <- solve(data, ...)
    z$setinverse(m1)
    m1  
    }
