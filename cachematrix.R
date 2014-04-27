## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    matrix_inverse <- NULL

    get <- function() {
        # Return the matrix that was passed into the function
        # or set by set(data)
        return(x)
    }

    set <- function(data){
        # Assign a new matrix
        x <<- data
        matrix_inverse <<- NULL
    }

    get_inverse <- function() {
        # Return the inverse of the matrix
        # Null if not yet calculated (see first line of the function)
        return(matrix_inverse)
    }

    set_inverse <- function(inverse) {
        # Assign the inverse of the data matrix
        matrix_inverse <<- inverse
    }

    # Return the four embedded functions in a list so they can be manipulated
    return(list(get = get, set = set, get_inverse = get_inverse, set_inverse = set_inverse))

}


## Write a short comment describing this function

cacheSolve <- function(cache_matrix, ...) {
        ## Return a matrix that is the inverse of 'x'

        # First, check to see if the inverse has been calculated
        # If not, calculate and set it.
        inverse <- cache_matrix$get_inverse()
        if (is.null(inverse)) {
            # Get the inverse
            print("Calculating inverse")
            inverse_matrix <- solve(cache_matrix$get())
            cache_matrix$set_inverse(inverse_matrix)
        }
        # Return the inverse
        return(cache_matrix$get_inverse())
}
