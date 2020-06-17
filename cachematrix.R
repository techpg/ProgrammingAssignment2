## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL                                 ##Assigning i the value of null. It will hold the value of the inverse.
    set <- function(y) {                      ## Defining the set function
      x <<- y
      i <<- NULL                              ## If there's a new matrix, reset i to null
    }
    get <- function() x                       ## Defining the get function. returns the matrix itself
    
    setinv <- function(inverse) i <<- inverse ## sets the value of i in parent environment
    getinv <- function() i                    ## gets the value of the inverse when called
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)                     ## Making a list of all the functions which will be returned as the output of makeCacheMatrix

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()                         ## gets the value of i stored in the above vector
  if(!is.null(i)){
    message("Getting Cached Data")
    return(i)
  }                                       ## Conditional loop for getting the already computed inverse if it exits
  x1 <- x$get()                           ## getting the matrix for inverse computation
  i <- solve(x1,...)                      ## Computing the inverse of the matrix x
  x$setinv(i)
  i

}
