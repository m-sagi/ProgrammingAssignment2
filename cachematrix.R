## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix function.

##Creates a special "matrix" object that can cache its inverse
## <<-

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    get <- function() x
    setInverse <- function(Inverse) inverseMatrix <<- Inverse
    getInverse <- function() inverseMatrix
    list(setMatrix = set, getMatrix = get,
    setInverse = setInverse,
    getInverse = getInverse)
    
}


##Computes the inverse of the special "matrix" returned by makeCacheMatrix
##use "solve" function to compute inverse of a matrix

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    inverseMatrix <- x$getInverse()
    
    ## Check for the cached data
    if(!is.null(inverseMatrix)) {
        message("getting cached data")
        return(inverseMatrix)
    }
    
    ## Compute inverse of a new matrix
    newMatrix <- x$getMatrix()
    inverseMatrix <- solve(newMatrix, ...)
    x$setInverse(inverseMatrix)
    inverseMatrix
}
