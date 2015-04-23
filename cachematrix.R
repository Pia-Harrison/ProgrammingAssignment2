## a pair of functions that cache the inverse of a matrix

## the function makeCacheMatrix stores four functions:
## 1.set() function: changes the matrix stored in the main function to the y input from the main function & m sets any 
## previously saved matrix to NULL so that the inverse matrix can be recalculated from the y input provided
## 2.get() function: returns the matrix stored in the main function
## 3.setinverse() function: stores the matrix in a variable m into the main function makeCacheMatrix
## 4.getinverse() function: stores teh matrix in a variable m into the main function and returns it

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function()x
  setinv <- function(solve) m <<- solve
  getinv <- function()m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## the cacheSolve function uses the functions in makeCacheMatrix to checks 
## if there is an inverse matrix stored in the 'cache'; otherwise,
## it calculates the inverse of the matrix
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {

  m <- x$getinv()
  if(!is.null(m)){
      message("getting cached data")
      return(m)
  }
  data <- x$get()          ## gets the matrix saved in the main function as x
  m <- solve(data, ...)    ## m calculates the inverse matrix here!
  x$setinv(m)              ## stores the inverse matrix in the object created in makeCacheMatrix
}

m <- matrix(c(-1,-2,1,1), 2,2)
x <- makeCacheMatrix(m)
x$get()


inv <- cacheSolve(x)
inv

inv <- cacheSolve(x)
