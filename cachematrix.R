## The combination of the makeCacheMatrix and cacheSolve functions eliminates
## the necessity of recalculating the inverse matrix of a square (i.e. 2 x 2)
## invertible matrix repeatedly as long as the matrix doesn't change. The fun-
## ctions do this by caching the inverse matrix results in virtual memory. When
## the functions are run, a check is performed on the source matrix and, if it
## hasn't changed since the last results were calculated/cached, the cached
## results will be retrieved (this will generally increase performance overall).
## Otherwise, a new inverse matrix will be calculated on the new source matrix
## and then re-cached.


## This function takes an invertible matrix as an argument. It constructs four
## other functions (get, set, getinverse, setinverse) and generates a list con-
## taining the four functions along with the x and m variables. These functions
## (chiefly the getinverse and setinverse) are used by the cacheSolve function
## to perform the use cache or recalulate test.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}

I2 <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)
mdat <- matrix(c(1, 2, 3, 11), nrow = 2, ncol = 2)

my_matrix <- makeCacheMatrix(m1)


## This function takes the object created when makeCacheMatrix is run as an
## argument. This function primarily determines whether the cached results
## can be used (no change in matrix) or the results need to be recalculated
## and cached (matrix is different).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if (!is.null(m)) {
		message("getting cached data")
		return(m)
	}
		data <- x$get()
		m <- solve(data, ...)
		x$setinverse(m)
		m

}

cacheSolve(my_matrix)


