## This file stores functions are able to calculate and cache the
## inverse matrix for given matrix

## Create a list, which contains functions to set/get source matrix
## and it's inverse matrix

makeCacheMatrix <- function(given_matrix = matrix()) {

        inverse_matrix <- NULL
        set <- function(new_matrix) {
                given_matrix   <<- new_matrix
                inverse_matrix <<- NULL
        }
        get <- function() given_matrix
        set_inverse <- function(new_inverse) inverse_matrix <<- new_inverse
        get_inverse <- function() inverse_matrix
        list(set = set, get = get,
               set_inverse = set_inverse,
               get_inverse = get_inverse)

}


## Return a matrix that is the inverse of 'given_matrix'

cacheSolve <- function(given_matrix, ...) {

        inverse_matrix <- given_matrix$get_inverse()
        if(!is.null(inverse_matrix)) {
                message("getting cached data")
                return(inverse_matrix)
        }
        data <- given_matrix$get()
        inverse_matrix <- solve(data, ...)
        given_matrix$set_inverse(inverse_matrix)
        inverse_matrix

}





