## Put comments here that give an overall description of what your
## functions do

# These functions work together to create a cache mechanism for the inverse of a matrix, as specified in the assignment instructions.

## Write a short comment describing this function

# inverse <- NULL: Initializes a variable inverse to store the cached inverse.
# set <- function(y) { ... }: Defines a function set to set the value of the matrix (x) and reset the cached inverse (inverse).
# get <- function() x: Defines a function get to retrieve the value of the matrix.
# setInverse <- function(solve) inverse <<- solve: Defines a function setInverse to set the value of the inverse (inverse) and cache it.
# getInverse <- function() inverse: Defines a function getInverse to retrieve the cached inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inverse <<- solve
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

# inverse <- x$getInverse(): Retrieves the inverse from the cache.
# if (!is.null(inverse)) { ... }: Checks if the inverse is already cached.
# message("getting cached data"): Displays a message indicating that cached data is being retrieved.
# data <- x$get(): Retrieves the matrix data.
# inverse <- solve(data, ...): Computes the inverse of the matrix.
# x$setInverse(inverse): Caches the inverse.
# inverse: Returns the inverse.

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}

# Create a sample matrix
sample_matrix <- matrix(c(1, 2, 3, 4), nrow = 2)

# Create a cache matrix object
cached_matrix <- makeCacheMatrix(sample_matrix)

# Retrieve the inverse of the sample matrix using cache matrix object
cached_matrix$getInverse()

# Retrieve the inverse of the sample matrix using cacheSolve
cacheSolve(cached_matrix)

# Compute the actual inverse of the sample matrix
actual_inverse <- solve(sample_matrix)

# Compare the cached inverse with the actual inverse
identical(cacheSolve(cached_matrix), actual_inverse)
