## Create the 4 function that will cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y = matrix()) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_inv <- function(inverse = matrix()) inv <<- inverse
  get_inv <- function() inv
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}


## Check if matrix have been calculated before, 
## if true return the cache value, if false calculate the value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$set_inv(inv)
  inv
}
