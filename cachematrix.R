# This programming assignment, demonstrate how caching can potentially save computation time.
# Taking the inverse of a matrix can take quite a long time for large matrices. 
# This can be resource draining if it has to be computed repeatedly, like in a loop.
# If the content of the matrix is not changing, it makes sense to reuse the already 
# calculated and cached inverse of the matrix, when needed, rather than recomputing it.
# This time it can be retrieved from the cache at will. While doing this
# the assignment will take advantage of the scoping rules in R. It will demonstrate how 
# to preserve a state of an R object.
#
# The following two functions are used to cache the inverse of a matrix.

########################################################
# makeCacheMatrix 
#
# Purpose: Function to: 
# set and get the value of a matrix, as well as 
# set and get the value of the inverse matrix
########################################################
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    ## Set the value of the matrix and create a place holder for the inverse matrix
	## within the scope of the function
	set <- function(y) {
        x <<- y
        inv <<- NULL
    }
	## Get the matrix 
    get <- function() {
	    x
    }
	## Set the inverse matrix
    setinverse <- function(inverse) {
	    inv <<- inverse
    }
	## Get the inverse matrix
    getinverse <- function() {
	    inv
	}
	## List contains set, get, setinverse and get inverse
    return(list(set=set, get=get, setinverse=setinverse, getinverse=getinverse))
}

############################################################## 
# cacheSolve
#
# Purpose: Returns the inverse of the matrix. if
# the inverse has already been computed, it gets the 
# result from cache and skips the computation.  OtherwiseIf 
# computes the inverse and sets the value in the cache 
# memory using the setinvese function 
#
# This function assumes that the matrix is always invertible.
##############################################################
cacheSolve <- function(x, ...) {
    ## for a given matrix get i's inverse
    inv <- x$getinverse()
	## if the inverse was calculated before get it from the cache
     if(!is.null(inv)) {
        message("Getting the inverse cached matrix.")
        return(inv)
    }
	## Otherwise get the matrix
    data <- x$get()
	## Get the inverse of the matrix
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
##########################################
## Results based on the following  sample 
## data:
##       [,1] [,2]
## [1,]    3    2
## [2,]    1    1
##########################################
## 
## > x <- matrix(c(3,1,2,1),nrow=2,ncol=2)
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1] [,2]
## [1,]    3    2  
## [2,]    1    1
##
## In the first time we run the function there will be no cached results
## > cacheSolve(m)
##      [,1] [,2]
## [1,]    1   -2
## [2,]   -1    3
##
## In the next iteration the result is already cached, therefore the result will come from there
## > cacheSolve(m)
## Getting the inverse cached matrix.
##    [,1] [,2]
## [1,]    1   -2
## [2,]   -1    3
## 