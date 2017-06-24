## Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation.
## There may be some benefit to caching the inverse of a matrix rather than compute it repeatedly
## Below are two functions that are used to create a special object that stores a matrix and cache's its inverse

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##	1	set the value of the matrix
##	2	get the value of the matrix
##	3	set the value of its inverse
##	4	get the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



##  The following function calculates the inverse of the special "matrix" created with the above function. 
##  It first checks to see if the inverse has already been calculated. 
##  If so, it gets the inverse from the cache and skips the computation. 
##  Otherwise, it calculates the inverse of the matrix and sets the value of the matrix in the cache via the setmean function.

cacheSolve <- function(x, ...) {
       inv <- x$getInverse()
       if(!is.null(inv)) {
               message("getting cached data")
               return(inv)
        }
       data <- x$get()
       inv <- solve(data, ...)
       x$setInverse(inv)
       inv
}
