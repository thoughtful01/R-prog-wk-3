## A pair of functions that compute and cache the inverse of the matrix

## makeCacheMatrix creats a special matrix, which is a function to
## set/get the matrix
## set/get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
         inv <- NULL
         set <- function(y) {
                 x <<- y
                 inv <<- NULL
         }
         get <- function() x
         setinv <- function(invtemp) inv <<- invtemp
         getinv <- function() inv
         list(set = set, get = get,
              setinv = setinv,
              getinv = getinv)
}


## cacheSolve computes the inverse of the special matrix
## returned by makeCacheMatrix above, or
## retrieves the inverse from the cache if already calculated

cacheSolve <- function(x, ...) {
         inv <- x$getinv()
         if(!is.null(inv)) {
                 message("getting cached data")
                 return(inv)
         }
         data <- x$get()
         inv <- solve(data, ...)
         x$setinv(inv)
         inv
}
