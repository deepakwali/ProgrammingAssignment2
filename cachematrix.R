## Welcome to Programming Assignment 2

## This is the function which makes and stores the cache matrix.
## Input of this function is a matrix which then returns a list of functions, 
## that can be applied to this matrix or the others stored in your workspace.
## 'set' takes the matix, 'get' return it, 'seti' sets inverse of matrix and 'geti' returns it's inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	setM <- function(y){
		x <<- y
		inv <<- NULL
	}
	getM <- function() x
	setInv <- function(solve) inv <<- solve
	getInv <- function() inv
	list( set = setM, get = getM, seti = setInv, geti = getInv)
}


## cacheSolve first checks if the Inverse of the matrix has already been calculated
## and returns it without any computation if it exists in the cache, else it sets
## it again and returns it.

cacheSolve <- function(x, ...) {
	inv <- x$geti()
	if(!is.null(inv)){
		message('getting cached data')
		return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$seti(inv)
	inv
}
