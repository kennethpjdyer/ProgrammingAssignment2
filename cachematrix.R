###############################################################
# cachematrix.R - Submission for R Programming: Programming   #
#   Assignment No. 2. This program provides two functions for #
#   for use in creating and solving cached matrices.  The     #
#   first, makeCacheMatrix, takes a square matrix and returns #
#   a special matrix object.  The second, cacheSolve, takes   #
#   aforementioned special matrix object and returns the      #
#   solution, caching it in memory to save processing time    #
#   for future use.                                           #
#                                                             #
# Author: Kenneth P. J. Dyer. Richard Peng                    #
# Version: 0.2                                                #
# Date: 23 May 2015                                           #  
###############################################################

#############################################################
# Function initializes matrix variable, returns a list of
#   cached functions to help speed up processing.
makeCacheMatrix <- function(x = matrix()) {

    # Initialize Variable
    inv <- NULL

    # Initialize Functions
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv

    # Return Subscription List
    return(list(set = set, get = get,
                getinverse = getinverse,
                setinverse = setinverse))
}

###############################################################
# Function to solve square matrices, draws from cached object
#   where available.  Returns the inverse of the cached matrix
#   it receives.
cacheSolve <- function(x, ...) {
    
    # Initialize inv with cache
    inv <- x$getinverse()

    # Return cache if available
    if (!is.null(inv)) {
        message("Loading cached data...\n")
        return(inv)
    }

    # Initialize empty cache object
    data <- x$get()
    
    # Find inverse
    inv <- solve(data,...)

    # Cache inverse
    x$setinverse(inv)

    # Return inverse
    return(inv)
}



