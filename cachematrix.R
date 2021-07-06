makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL ## As in example but renamed variables and changed function 
   set <- function(y) {
      x <<- y
      inv <<- NULL
   } 
   ## Function to "remember" new matrix
   get <- function() x
   ## Get back the matrix
   setinv <- function(inver) inv <<- inver
   ## Set inverse of matrix 
   getinv <- function() inv
   ## Get inverse of matrix 
   list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
   ## Create list of functions 
}
cacheSolve <- function(x, ...) {
   inv <- x$getinv()
   if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
   }
   data <- x$get() 
   ## Setting new matrix 
   inv <- solve(data, ...)
   ## Getting a result 
   x$setinv(inv)
   ## Remembering the result
   inv
}
