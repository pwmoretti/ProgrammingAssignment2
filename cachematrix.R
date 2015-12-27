## A square matrix "x" must be used
## below is show how to build a random square matrix
## in the range [0-10]
## matrix(runif(9, min=0, max=10),3,3)
##
##
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                ## assign to an object "x" of parent environment the value of y
                ## assign to an object "m" of parent environment the value NULL
                ## reading the x value from y argument
                x <<- y
                inv <<- NULL
        }
        ## define an anonymous function get() as the value of x
        get <- function() x
        ## define an anonymous function setinv() setting the value of x to inverse
        ## of matrix "x", using "solve()" base function
        setinv <- function(solve) inv <<- solve
        ## define an anonymous function getinv() the value reading the value of
        ## inv
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## The "cacheSolve" function will search in parent environment for x
## set the value of "getinv()" function to an object "inv"
## checks if inv is not null, if not null print the message "getting..."
## and return the "inv" values of inverted matrix solved before
## if the "inv" value is null, then solve the matrix "x" to evaluate the
## inverse.

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


## example:
## create a random sqare matrix m as
## m <- makeCacheMatrix(matrix(runif(4, min=0, max=10),2,2))
##
## run cacheSolve() function to invert "m"
## cacheSolve(m)
## an output like the following will be shown:
##      > cacheSolve(m)
##      [,1]      [,2]
##      [1,] -1.886577  2.036619
##      [2,]  1.474313 -1.470280
##
## running again, the cache "inv" is not null and the output it changes to:
##
##      > cacheSolve(m)
##      getting cached data
##      [,1]      [,2]
##      [1,] -1.886577  2.036619
##      [2,]  1.474313 -1.470280
##