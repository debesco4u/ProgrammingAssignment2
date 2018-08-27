##The functions create a special matrix object and cache the inverse of a matrix.

## Write a short comment describing this function
##makeCacheMatrix: This function creates a special "matrix" object that can 
##cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
 		invmat <- NULL
        	set <- function(y) {
                		x <<- y
                		invmat <<- NULL

	}
	get <- function() x
      setinvmat <- function(inverse) invmat <<- inverse
      getinvmat <- function() invmat
      list(set = set, get = get,
             setinvmat = setinvmat,
             getinvmat = getinvmat)
}


## Write a short comment describing this function
##cacheSolve: This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve the 
##inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  invmat<- x$getinvmat()
        if(!is.null(invmat)) {
                message("getting cached data")
                return(invmat)
        }
        data <- x$get()
        invmat <- solve(data, ...)
        x$setinvmat(invmat)
        invmat
}


