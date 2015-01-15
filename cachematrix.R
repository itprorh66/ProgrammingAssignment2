# Name:        CacheMatrix
# Purpose:     Computes the inverse of a matrix using caching to improve
#              performance by implementing the 'Closure' functionality
#
# Created:     14/01/2015
#
# Licence:     GNU General Public License, version 3 (GPL-3.0)
#
#              This program is distributed WITHOUT ANY WARRANTY;
#              without even the implied warranty of MERCHANTABILITY
#              or FITNESS FOR A PARTICULAR PURPOSE.
#-------------------------------------------------------------------------------

## This function accepts a matrix as input and creates a function to cache
## the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
      get <- function() x
      setsolve <- function(solve) m <<- solve
      getsolve <- function() m
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}


## this function utilizes the features of makeCacheMatrix to solve for
## The inverse of a matrix utilizing a cache to improve performance
## by not recomputing a matrix element that has already been computed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

     m <-x$getsolve()
     if(!is.null(m)){
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m<- solve(data, ...)
     x$setsolve(m)
     m
}
