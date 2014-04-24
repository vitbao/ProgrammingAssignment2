## Programming Assignment 2

# Function makeCacheMatrix takes a predefined matrix and store its inversed matrix into cache

makeCacheMatrix <- function(x = matrix()) {
        # Initialize variable 'inverse' to NULL. 
        # This variable will store the inverse of the matrix 'x'
        inverse <- NULL
        
        # This function below is actually not needed for this assignment 
        # but it can be useful if we want to set a new matrix instead of creating a new one
        set_matrix <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        # Define function 'get_matrix' to get the predefined matrix 'x'
        get_matrix <- function() x
        
        # Define function 'set_inverse' to store the inversed matrix into cache once it's computed
        set_inverse <- function(cached_inverse) {
                inverse <<- cached_inverse
        }
        
        # Define function 'get_inverse' to get the inverse matrix from cache (if it exists)
        get_inverse <- function() inverse
        
        # Create a list of functions
        list(set_matrix = set_matrix, get_matrix = get_matrix,
             set_inverse = set_inverse, get_inverse = get_inverse)
}


# Function cacheSolve returns the inverse of a matrix that is first created by 
# function makeCacheMatrix

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        
        # First check if the inverse of matrix x is already computed and stored in cache
        inverse <- x$get_inverse()
        
        # If the inverse of this matrix is in cache, print message and return the inversed matrix
        if (!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        # If the inverse of this matrix is not yet computed and stored in cache, 
        # get the matrix and call built-in function 'solve' to compute the inversed matrix
        matrix <- x$get_matrix()
        inverse <- solve(matrix, ...)
        
        # store the inverse matrix in cache
        x$set_inverse(inverse) 
        
        # return the inversed matrix
        inverse
}

