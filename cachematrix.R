##----------------------------------------------------------------------------##
## Program assignment 2 by Marco Antonio Béjar Villalba
## From coursera Data Science Specialty.
##----------------------------------------------------------------------------##
## The project consists of 2 main functions and an utility function.
## makeCacheMatrix creates a list with the data needed to cache the result
## of calculating an inverse matrix.
##
## cacheSolve takes the list created in makeCacheMatrix and verifies if the
## inverse has been calculated, if so then returns the cached result, if no
## the inverse is calculated.
##
## verify_time only logs the time elpased.
##
## Is assumed that the matrix received is not a singular matrix and thus can
## be inverted.
##----------------------------------------------------------------------------##
## Example of use, note the time elapsed the first time of the calculation
## and the second

## > a1 <- c(3, 2, 5) 
## > a2 <- c(2, 3, 2) 
## > a3 <- c(5, 2, 4) 
## > ma<-rbind(a1,a2,a3)
## > ma
## [,1] [,2] [,3]
## a1    3    2    5
## a2    2    3    2
## a3    5    2    4
## > solve_matrix<- makeCacheMatrix(ma)
## > cacheSolve(solve_matrix)
## [1] "Time elapsed: -0.000984"
## a1          a2         a3
## [1,] -0.29629630 -0.07407407  0.4074074
## [2,] -0.07407407  0.48148148 -0.1481481
## [3,]  0.40740741 -0.14814815 -0.1851852
## > cacheSolve(solve_matrix)
## [1] "Time elapsed: 0.000000"
## a1          a2         a3
## [1,] -0.29629630 -0.07407407  0.4074074
## [2,] -0.07407407  0.48148148 -0.1481481
## [3,]  0.40740741 -0.14814815 -0.1851852
## > 

##----------------------------------------------------------------------------##
##  The code below caches the result of the inverse of a matrix.
##  The result of the function is a list with the functions to get and set
##  the matrix and its inverse. The list resembles an object with the 
##  functions stored in the list acting as methods.

makeCacheMatrix <- function(originalMatrix = matrix()) {
  
  # First asigns the initial value of the matrix as NULL
  inverseMatrix <- NULL 
  
  set <- function(aMatrix){
    # The matrix is set and the inverse matrix is also reset because
    # it does not corresponds to current computation of the matrix
    # that is being passed in. We use the super assignment operator to work 
    # with the parent environment not the environment inside this function.
    originalMatrix <<- aMatrix
    inverseMatrix <<-NULL
  }
  
  get <- function(){
    originalMatrix
  }
  
  setInverse <- function(inverse){
    #  I use the super assigment operator to set inverse_matrix in the parent
    #  environment.
    inverseMatrix <<- inverse
  }
  
  getInverse <- function(){
    inverseMatrix
  }
  
  # Returns the list with the functions above listed.
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##----------------------------------------------------------------------------##
## Caches the result for the inverse matrix of a given matrix

cacheSolve <- function(x, ...) {
  
  log_time <- verify_time()
  
  # Gets the inverse
  inverse <- x$getInverse()
  
  #If the inverse does exist return the cached value.
  if(! is.null(inverse)){
    # Logs the time elapsed.
    log_time()
    print("returns cached value.")
    ## Return a matrix that is the inverse of 'x'.
    return(inverse)
  }
  
  #If the inverse does not exist calculate the inverse and set it in the special
  #list
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inverse)
  
  # Logs the time elapsed.
  log_time()
  
  ## Return a matrix that is the inverse of 'x'
  inverse
}

##----------------------------------------------------------------------------##
# This function uses the same paradigm for getting the time elapsed in case of 
# a cached or not cached request.

verify_time <- function(){
  
  # Just to verify the time for cached and not cached request.
  start_time<-Sys.time()
  
  # Logs the string to log the time elpased
  function(){
    interval <- start_time - Sys.time()
    print(sprintf("Time elapsed: %f",interval))
  }
}
