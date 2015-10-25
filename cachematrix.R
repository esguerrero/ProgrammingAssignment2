## These functions implements something that in Java could be seen as 
## a "JavaBean" the so called: "makeCacheMatrix" accessing to properties 
## using getter and setter methods. And a "cache" function which takes a 
## "RBean" object, i.e., the makeCacheMatrix object and by using the 
## 'solve' function calculates the inverse of the matrix.

## RBean - a "bean" in R containing a matrix object
# subfunctions: set,get, setinv, getinv, list
makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     #setmean <- function(mean) m <<- mean
     setinv <- function(inv) m <<- inv
     #getmean <- function() m
     getinv <- function() m
     
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}



## Returns a matrix that is the inverse of 'x' obtaine from a RBean  
cacheSolve <- function(x, ...) {
     
     #use the function solve(x) x:square invertible matrix
     
     m <- x$getinv()
     if(!is.null(m)) {
          message("Getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setinv(m)
     m
}
