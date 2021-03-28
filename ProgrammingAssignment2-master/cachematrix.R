## makeCacheMatrix is a function that creates a special "matrix" object
## that can cache its inverse while cacheSolve computes the inverse of the matix

## This function takes a square invertible matrix "x" as an input and then
## returns a list of 4 functions to: set the matrix, get the matrix,
## set the inverse of the matrix and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      ##  returns a list of functions to set and get a matrix input
      ##  and set and get its inverse.

      inv <- NULL
      set <- function(y) {
          ## the double arrow (scoping) assignment is used to change the
          ##  global value of x and inv to what is defined in here.
              x <<- y
              inv <<- NULL
      }

      get <- funtion() {x}
      setInverse <- function(inverse) {inv <<- inverse}
      getInverse <- function() {inv}

      list(set = set,
          get = get
          setInverse = setInverse,
          getInverse = getInverse)

}


## cacheSolve takes makeCacheMatrix as an input, gets the inverse if it is
## already computed and computes if Otherwise,
## and passes the inverse to setInverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        ## gets the inverse of the matrix from makecacheMatrix
        inv <- x$getInverse()

        ## if the inverse is not NULL i.e it has already been calculated
        if(!is.null(inv)) {
        ## get it from the cache and skip the computation
               message("getting cached data")
               returns(inv)
        }

        ## else, compute the inverse of the matrix retrieved by get()
        mat <- x$get()
        inv <- solve(mat, ...)

        ## sets the computed inverse to setInverse function
        x$setInverse(inv)

         return(inv)

}
