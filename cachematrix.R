
## Caching the inverse of a matrix
## January 25, 2015
## MakeCacheMatrix is a function which create a square matrix object that can catche its inverse. This function is able to cache the inverse of a matrix without time
## consuming. Then catchSolve function computes the inverse of the square matrix returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.



##In this R function, I create a square matrix, makeCacheMatrix, which creates a special "matrix" object that can cache its inverse.
##The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverse
#4.  get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL 
  y <- NULL 
  set <- function(y) { 
    x <<- y 
    m <<- NULL 
  }
  get <- function() x
  setinverse <- function(inverse) m<<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## `cacheSolve` function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
#If the inverse has already been calculated (and the matrix has not changed), then`cacheSolve` should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) { 
  m<- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m<- solve(data, ...)
  x$setinverse(m)
  m
  
}

