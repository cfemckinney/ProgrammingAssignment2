## These functions work interactively to set and calculate the inverse
## of a matrix.

## This function sets the inverse of the matrix to NULL so it will 
## compute a new value, then sets the function y so that the argument
## x = y in this environment. It then lists each get and set function
## to be returned.

makeCacheMatrix <- function(x = matrix()) {
		inv = NULL
		set = function(y) {
			x <<- y
			inv <<- NULL
		}
		get = function() x
		setinv = function(inverse) inv <<- inverse
		getinv = function() inv
		list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function retrieves the inverse from the makeCacheMatrix 
## function, and looks to see if there is a NULL value. If there is a
## NULL, it returns the inverse of the matrix, if there is not a NULL,
## it sets the inverse from the makeCacheMatrix function and returns
## the inverse.

cacheSolve <- function(x, ...) {
       inv = x$getinv()
       if(!is.null(inv)){
       	message("getting cached data")
       	return(inv)
       }
       data = x$get()
       inv = solve(data, ...)
       x$set(inv)
       return(inv)
}
