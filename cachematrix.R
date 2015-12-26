## Put comments here that give an overall description of what your
## functions do

## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x # printing the matrix in the console after we used our function makeCachematrix()
  setsolve <- function(solve) inv <<- solve #caution with this command line
  getsolve <- function() inv #looking for the result in cached data
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

cacheSolve <- function(x, ...) {
  inv <- x$getsolve() 
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  } # if we already have a result for the inverse of the matrix, we just use our cached data
  data <- x$get() #getting the matrix which we want to invert
  inv <- solve(data, ...) #inverting the matrix
  x$setsolve(inv) 
  inv #printing the result in the console
}

a <- makeCacheMatrix(matrix(nrow=2,ncol=2,c(2,0,0,2)))
a$get()
a$getsolve()
cacheSolve(a)
cacheSolve(a)
a$getsolve()
a$set(matrix(nrow=2,ncol=2,c(10,0,0,20)))
a$getsolve()
cacheSolve(a)
