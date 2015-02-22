## The following functions are written to simplify the procedure of 
## calculating the inverse of a matrix, as inversion is a costly procedure
## in terms of computation time

## makeCacheMatrix function is creates a list of available functions that
## apply on our cach-able matrix. The functions are responsible for:
##  1. setting the value of the matrix
##  2. getting the value of the matrix
##  3. setting the corresponding inverse matrix
##  4. getting the corresponding inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
          x <<- y
          inv <<- NULL
        }
        get <- function() x
        setInv <- function(INV) inv <<- INV
        getInv <- function() inv
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve is taking makeCacheMatrix object as an argument and it returns
## the inverse of the matrix contained in this object, checking if it is 
## already calculated or not to save computations

cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        if(!is.null(inv)){
          message("Taking value from cache")
          return(inv)
        }
        newInv <- solve(x$get())
        x$setInv(newInv)
}
