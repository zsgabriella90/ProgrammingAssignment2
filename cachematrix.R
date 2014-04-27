#  Author: Gabriella Zsebehazi

#  Coursera - R Programming
#  Programming Assignment 2

############################################################################

# These functions serve to give the inverse of a matrix in an efficient way.
# The inverse of a given matrix is computed only once, then the value is
# stored and is simply cached next time instead of recomputing it.  

###################################
#      Function makeCacheMatrix
###################################

# This function creates a list, which contains 4 functions dedicated to
#    1.) (set) store the given matrix 
#    2.) (get) extract the matrix
#    3.) (setsolve) store the inverse of matrix
#    4.) (getsolve) extract the inverse of matrix    

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) inv <<- solve
        getsolve <- function() inv
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


########################################
#          Function cacheSolve
########################################

# This function searches the inverse of the matrix from the list created with the first function
# and returns the value if it exists. If the result is not available, the function 
# extracts the matrix, computes the inverse, stores the result and prints it. 

# The input of this function is the output list of the first function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inv <- x$getsolve()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setsolve(inv)
        inv
}
