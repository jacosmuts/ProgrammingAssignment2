## Two functions to cache the inverse (solve) of a matrix
## makeCacheMatrix to create special object containing matrix
##   and cache functions
##  cacheSolve that will use above matrix to calcuate inverse of matrix
##    or retrieve cache value of matrix inverse

## makeCacheMatrix:
##   returns a list with:
##     set, function to set the value of the matrix
##     get, function to get the value of the matrix
##     setsolve, function to set the inverse of the matrix (solve)
##     getsolve, function to get the cached inverse of the matrix (solve)

makeCacheMatrix <- function(x = matrix()) {
    mi <- NULL
    set <- function(y) {
        x <<- y
        mi <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) mi <<- solve
    getsolve <- function() mi
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve:
##   check if value of inverse (solve) is set
##   if not value of inverse is NULL
##     print message returning cached value
##     retrn cached value of inverse
##   else (continue after if loop....)
##     calculate inverse
##     set cache to inverse value <<-
##     return inverse

cacheSolve <- function(x, ...) {
    mi <- x$getsolve()
    if(!is.null(mi)){
        message("getting cached data")
        return(mi)
    }
    data <- x$get()
    mi <- solve(data,...)
    x$setsolve(mi)
    mi    
}
