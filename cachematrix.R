## These functions create a special matrix object (first function)
## in order to cache the inverse of a matrix in it.
## The inverse is computed OR retrieved by the second function.

## makeCacheMatrix function takes one argument which is a matrix
## and create a "special matrix object" which is a list of four function :
## - set : set the values of the matrix (by calling special_matrix_object$set(a_matrix))
## - get : get the values of the matrix
## - setinv : set the values of the inverse of the matrix (by calling special_matrix_object$set(matrix))
##   (inverse matrix will be provided by the cacheSolve function, if it has not been computed yet)
## - getinv : get the values of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inv_matrix) inv <<- inv_matrix
        getinv <- function() inv
        list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## cacheSolve function takes an argument x which is a "special matrix object" produced by the previous function,
## (and also further arguments "...")
## It checks if the inverse matrix has already been computed :
## (if the original matrix has been modified using special_matrix_object$set function, 
## then the inverse matrix is NULL and it needs computing)
## - if it has not been computed, then the function compute it using solve function, and call setinv function 
## to cache the inverse matrix into the "special matrix object"
## - if it has already been computed, it prints a message and retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv
}
