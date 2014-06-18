## The functions create cache matrix, estimates its inverse and stores into cache
## if the same matrix is supplied, returns already stored value and shows the message, if not, then it calculates again.

## Creates the matrix and then stores an estimated inverse matrix

makeCacheMatrix	<-	function(x = matrix())	{
			inv <- NULL
			set <- function(y)  {
					x <<- y
					inv <<- NULL
			}
			get <- function () x
			setinv <- function(inverse) inv <<- inverse
			getinv <- function() inv
			list(set = set, get = get,
				setinv = setinv,
				getinv = getinv)

}


## Estimates an inverse matrix and tests if the same matrixs is supplied and if already stored inverse matrix should be recieved

cacheSolve	<-	function(x, ...) {
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
