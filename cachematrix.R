## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##The makeCacheMatrix function returns a list element that contains 
##four structure values (or inner functions) get, set, getSolveInv, setSolveInv 


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolveInv <- function(solve) m <<- solve
  getSolveInv <- function() m
  list(set = set, get = get,
       setSolveInv = setSolveInv,
       getSolveInv = getSolveInv)
}


## Write a short comment describing this function

##The cacheSolve function checks if the inverse matrix has been cached
##so first time you run the cacheSolve function, it calculates the
##inverse of the matrix and sets it to the value
##Subsequent calls to cacheSolve will just return the cached data


cacheSolve <- function(x, ...) {
  m <- x$getSolveInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolveInv(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
