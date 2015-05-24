# Pair of functions which allow creating a matrix which keeps a cache of its
# inverse.

## Creates a matrix object which caches the inverse of itself.

makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL #cache
  set<-function(y) {
    x <<- y
    inverse <- NULL #Clean the previous value if exists
  }
  get <- function() {
    x
  }
  setinverse <- function(i) {
    inverse <<- i
  }
  getinverse <- function() {
    inverse
  }
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse
  )
}


## Implements the inverse of the matrix for the object created with the previous
#  function. If the inverse was previously calculated and cached it will return
#  the cached inverse otherwise it will calculate and return the inverse.

cacheSolve <- function(x, ...) {
  #Check cache
  i <- x$getinverse()
  if(!is.null(i)) { #Nothing to do
    message("Getting cached inverse of matrix")
  } else { #Calculate and store
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
  }
  return(i)
}
