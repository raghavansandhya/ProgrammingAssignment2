## These functions are to help cache(store) data of a matrix inverse
## as it is a costly operation. IT stores the value of the inverse
## for the first time and then retrieves it when called during later instances.

## The below fuction takes a matrix as the input parameter. The value 
## returned is a list of functions. 
## Sample Function Call: matrixVector <- makeCacheMatrix(exampleMatrix) 
## Sample Sub-Function Call: matrixVector$get()

makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
	        set <- function(y) {
	                x <<- y
	                inv <<- NULL
	        }
	        get <- function() x
	        setinv <- function(inverse) inv <<- inverse
	        getinv <- function() inv
	        list(set = set, get = get, 
	        	setinv = setinv,
             		getinv = getinv)

}


## The below fuction takes the value returened by makeCacheMatrix as the 
## input parameter. The value returned is the inverse of the matrix passed to makeCacheMatrix
## function. If it is the first time the inverse of the matrix is being calculated, returns
## the calculated value else returns the cached value.
## Sample Function Call: cacheSolve(matrixVector) 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'mat' used in the function makeCacheMatrix and returned to 'x'
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
