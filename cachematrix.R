# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(matrix = matrix()) {
  # Initialize the inverse property
  inv <- NULL
  
  # Method to set the matrix
  set <- function(y) {
    matrix <<- y
    inv <<- NULL  # Reset the inverse when the matrix is reset
  }
  
  # Method to get the matrix
  get <- function() {
    matrix
  }
  
  # Method to set the inverse of the matrix
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  # Method to get the inverse of the matrix
  getInverse <- function() {
    inv
  }
  
  # Return a list of methods
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Function to compute the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  # Get the cached inverse if it exists
  inv <- x$getInverse()
  
  # Return the cached inverse if it is already set
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Get the matrix from the special "matrix" object
  matrix <- x$get()
  
  # Calculate the inverse of the matrix
  inv <- solve(matrix, ...)
  
  # Cache the inverse
  x$setInverse(inv)
  
  # Return the inverse
  inv
}

