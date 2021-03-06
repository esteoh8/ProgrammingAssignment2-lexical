## cachematrix.R contains 2 functions makeCacheMatrix and cacheSolve which makes use of 
## R's lexical scoping to provide cacheing of the inverse of a matrix
## such that repeat calls to calculate the inverse the same matrix can be very quickly 
## satisfied from the "cache" 

## makeCacheMatrix assumes the input matrix is invertable
## makeCacheMatrix creates a function that does the following:
## 1.set the matrix
## 2.get the matrix
## 3.calculate, then set the Inverse of the matrix
## 4.get inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL             # inverseMatrix is the inverse matrix initially set to NULL
  set <- function(y) {              # 
    x <<- y                         # The special assignment operator "<<-" assigns the y value
    inverseMatrix <<- NULL          # into the parent environment...
  }										                      
  get <- function() x
  setInverse <- function(solve) inverseMatrix <<- solve
  getInverse <- function() inverseMatrix
  list(set = set, get = get, setInverse = setInverse,   # format:  set1=set1 
       getInverse = getInverse)                         # return list where the list items are named
}                                                       # and the name can be invoke using "$" extract
                                                        # operator

## cacheSolve works with makeCacheMatrix by supplying the cached inverse matrix if it was computed
## earlier.  If the cache is NULL, cacheSolve will solve() for the inverse, cache it and 
## return the inverseMatrix

cacheSolve <- function(x, ...) {           # Return inverse of x either from cache or solve() it
        
  inverseMatrix <- x$getInverse()          # inverseMatrix = NULL, set by makeCacheMatrix
  if(!is.null(inverseMatrix)) {            # so if not NULL, use the cached inverseMatrix
    message("getting the cached data")
    return(inverseMatrix)
  }
  tmp <- x$get()                           # if null (not cache), get matrix 
  inverseMatrix <- solve(tmp, ...)         # solve() for the inverse matrix 
  x$setInverse(inverseMatrix)              # save it to the cache it
  inverseMatrix                            # and return the inverseMatrix 
}