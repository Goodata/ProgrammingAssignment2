
## The makeCacheMatrix function creates a special 'matrix'
## The matrix is a list that contains a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	        i <- NULL 
	        set <- function(y) {
	                x <<- y
	                i <<- NULL 
	    }
            get <- function() x
            setinverse <- function(inverse) i <<- inverse
            getinverse <- function() i
	        list(set = set, get = get,
	             setinverse = setinverse, 
	             getinverse = getinverse) 
}


## The cacheSolve function calculates the inverse of the special 'matrix',
## The function checks to see if the inverse is calculated
## If TRUE, it gets the inverse from the cache and does not compute.
## Otherwise it computes the inverse of the 'matrix', and
## sets the inverse value with the setinverse function.
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }

        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

	       


