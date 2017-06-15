## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Creates four functions to set and get the matrix and its inverse
##	1. get() 2. set() 3. setinv() 4. getinv()

makeCacheMatrix <- function(x = matrix()) {
	x_inv <- NULL
	set <- function(y)
	 {
#		rows <- nrow(y)
#		cols <- ncol(y)
#		x <<- matrix(y, rows, cols)
		x <<- y
		x_inv <<- NULL
	}
	get <- function () x
	setinv <- function(inv)	x_inv <<- inv
	getinv <- function() x_inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## cacheSolve(x) reads the inverse of matrix. If the inverse is not NULL
## then it simply getinv(). Otherwise, it creates and caches inverse 
## and outputs the inverse it calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()

#Inverse is not NULL. Return inverse. Nothing more to do.
	if (!is.null(inv)) {
		message("getting cached data")
		return (inv)
	}


#Inverse is  NULL. Get the matrix and Solve().
	x_mat <- as.matrix(x$get())

#print(x_mat)
#print(dim(x_mat))

	if (nrow(x_mat) != ncol(x_mat)) {
		print("Not a square matrix. Inverse doesn't exist.")
	}

	else {

# Solve() throws some error when it can't find the inverse. Ideally, they need to be caught
# and the program handle it gracefully. It does seem to behave properly when things line up 
# well for it :-)
## Okay, I tried to wrap tryCatch() around it.. hopefully it behaves a bit nicer now..
		
		tryCatch(inv <- solve(x_mat), error = function (c)
			{
			print("Inversion failed.")
			return(NA)
			})
		x$setinv(inv)
		inv
	}
}


