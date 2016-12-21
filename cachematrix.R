## Put comments here that give an overall description of what your
## functions do


##The first function, `makeCacheMatrix` return an object, 
## which is a a list of functions:
##  1. set the value of the matrix;
##  2. get the value of the matrix;
##  3. set the value of the inverse;
##  4. get value of the inverse;
makeCacheMatrix <- function(x = matrix()) {
        iv <- NULL
        set <- function(y) {
                x <<- y
                iv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) iv <<-inverse
        getInverse <- function() iv
        list(set = set, get = get, setInverse = setInverse,
             getInverse = getInverse)
 
}



##The following function try to solve the inverse matrix in the 
##object created by makeCacheMatrix function.
##However, it checks to see if the inverse of the matrix has been
##calculated. If so, it `get`s the inverse from the
##cache and skips the computation. Otherwise, it calculate the 
##reverse, and sets the value of the inverse to the object in the 
##catche via 'setInverse' function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ##First check if the inverse has been calculated
        iv <- x$getInverse()
        if (!is.null(iv)) {
                message ("getting cached data")
                return(iv)
        }
        #if inverse hasn't been calculated, then calculate
        iv <- solve(x$get(), ...)
        #set iv to x
        x$setInverse(iv)
        iv
}
