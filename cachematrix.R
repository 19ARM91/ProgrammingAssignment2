## This pair of functions is designed to cache the inverse of a matrix.
## Matrix inversion is a computationally expensive operation, so caching the 
## result can save time when the inverse of the same matrix is needed multiple times.
## This function creates a special "matrix" object that can cache its inverse.
## It includes methods to set and get the matrix, as well as to set and get its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize the inverse as NULL
    
    # Method to set the matrix
    set <- function(y) {
        x <<- y   # Assign the new matrix value to x in the parent environment
        inv <<- NULL  # Reset the inverse when the matrix is changed
    }
    
    # Method to get the matrix
    get <- function() x  # Return the matrix
    
    # Method to set the inverse of the matrix
    setInverse <- function(inverse) inv <<- inverse
    
    # Method to get the cached inverse of the matrix
    getInverse <- function() inv
    
    # Return a list of the methods
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## it retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()  # Get the cached inverse, if available
    
    # If the inverse is already cached, return it
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # Otherwise, calculate the inverse of the matrix
    mat <- x$get()  # Get the original matrix
    inv <- solve(mat, ...)  # Calculate its inverse using the solve function
    
    # Cache the calculated inverse for future use
    x$setInverse(inv)
    
    # Return the inverse
    inv
}
