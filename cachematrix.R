## In this program we are defining two functions makeCacheMatrix()
## and cacheSolve() to check whether inverse of a matrix is stored
## in the cache or not. If not then we are storing the inverse in the
## cache and then checking whether when next the function is called
## the inverse is called from the cache or computed. Ideally, if the
## inverse is computed once it should be get from the cached data
## and not be computed again.

## makeCacheMatrix(): this function basically returns a list of functions
## designed to do different activities. set() and get() are used for
## setting and getting the matrix of which inverse is to be found out.
## setsolve() is used for storing the inverse into the cache and getsolve()
## is to get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)

}


## cacheSolve(): The list of functions that we will get from the above function will be 
## passed as an argument in the function below. s is a variable which will
## assigned with a value of inverse then we will check whether the assigned 
## value is NULL or not. If it's not NULL that means it has been computed
## already and which means it can be recalled from the cached data. 
## And if s is NULL that means it needs to be computed once and stored into
## the cache for next set of iterations.

cacheSolve <- function(x, ...) {
  
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  matrix <- x$get()
  s <- solve(matrix, ...)
  x$setsolve(s)
  s                    ## Return a matrix that is the inverse of 'x'
}


