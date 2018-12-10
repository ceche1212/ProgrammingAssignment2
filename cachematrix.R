## creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

inicio <- NULL

    ## Set the initial matrix
    set <- function( matrix ) {
            mat <<- matrix
            inicio <<- NULL
    }

    ## Get the matrix
    get <- function() {
    	## Return the matrix
    	mat
    }

    ## Set the inverse of the matrix
    setInverse <- function(inverse) {
        inicio <<- inverse
    }

    ## Get the inverse of the matrix
    getInverse <- function() {
        ## Return the inverse property
        inicio
    }

    ## Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)


}


## return the inverse of a matrix, if the inverse has already been calculated
## the function should return the inverse of the cache

cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    mat <- x$getInverse()

    ## Just return the inverse if its already set
    if( !is.null(mat) ) {
            message("getting cached data")
            return(mat)
    }

    ## Get the matrix from our object
    data <- x$get()

    ## Calculate the inverse using matrix multiplication
    mat <- solve(data) %*% data

    ## Set the inverse to the object
    x$setInverse(mat)

    ## Return the matrix
    mat
}
