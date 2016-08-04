## Name: Chris Lydick
## Date: 8/4/2016
## Description: This R file contains two functions, 
##		makeCacheMatrix(x) where x is the matrix you wish to embed
##			into the object. The object is returned.
##		cacheSolve(x) where x is the object created from makeCacheMatrix()
##			and 

## This function creates an object containing the matrix passed (or set) and
##   also contains the inverse which is initialized to NULL. 
makeCacheMatrix <- function(x = matrix()) {

    # 'inv' is the inverse of the matrix passed as 'x'
    inv <- NULL
    
    # 'set' is the function to assign a matrix value within the object
    set <- function (y) {
      x <<- y
      inv <<- NULL
    }
    
    # 'get' retrieves the value of the object 
    get <- function(y) x
    # 'setInverse' will set the inverse matrix within the object
    setInverse <- function(new_inv) inv <<- new_inv
    # 'getInverse' will get the inverse matrix within the object
    getInverse <- function() inv
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## This function modifies a matrix object created from the makeCacheMatrix() function
##   and solves for the inverse of the matrix (storing it into the object).
cacheSolve <- function(x, ...) {

    # 'inv' attempts to get the inverse matrix
    inv <- x$getInverse()

    # if the value is not null, then return the value and exit the function.
    if (!is.null(inv)) {
      message("getting cached inverse matrix")
      return(inv)
    }

    # Temporarily store the first matrix
    data <- x$get()
    # Attempt to solve(inverse) the matrix using solve(). Pass '...' through
    inv <- solve(data, ...)
    # Set the inverse of the object when known.
    x$setInverse(inv)
    # return the inverse matrix just solved.
    inv
  
}
