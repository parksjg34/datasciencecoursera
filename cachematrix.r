##Matrix inversion is usually a costly computation and their may be some benefit to 
##caching the inverse of a matrix rather than compute it repeatedly.
##Your assignment is to write a pair of functions that cache the inverse of a matrix.

##---------------------------------------------------

##  makeCacheMatrix creates a special Matrix, which is really a list containing a function to: 
##  1.set the value of the Matrix
##  2.get the value of the Matrix
##  3.set the value of the Inverse
##  4.get the value of the Inverse

##Class EXAMPLE##
  ##   makeVector <- function(x = numeric()) {
  ##      m <- NULL
  ##      set <- function(y) {
  ##        x <<- y
  ##        m <<- NULL
  ##      }
  ##      get <- function() x
  ##      setmean <- function(mean) m <<- mean
  ##      getmean <- function() m
  ##      list(set = set, get = get,
  ##           setmean = setmean,
  ##           getmean = getmean)
  ##     }


makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  setmatrix<-function(y){
    x <<- y
    m <<- NULL
  }
  getmatrix<-function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}

##---------------------------------------------------

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
##should retrieve the inverse from the cache.

##For this assignment, assume that the matrix supplied is always invertible.

##Class EXAMPLE##

  ##        cachemean <- function(x, ...) {
  ##          m <- x$getmean()
  ##          if(!is.null(m)) {
  ##            message("getting cached data")
  ##            return(m)
  ##          }
  ##          data <- x$get()
  ##          m <- mean(data, ...)
  ##          x$setmean(m)
  ##          m
  ##        }

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix <- x$getmatrix ()
  m <- solve(matrix)
  x$setinverse(m)
  m
}

##---------------------------------------------------

##    Sample:
##    
##    x = rbind(c(1, -2), c(-3, 4))       
##    g = makeCacheMatrix(x) 
##    g$getmatrix() 
    
##    cacheSolve(g)    ##Note: Will not see "getting cached data" in first run, but will appear in second+

