## Put comments here that give an overall description of what your
## functions do


## makeCacheMatrix sets up the matrix 'object' defining 4 different methods
## the 4 methods create and retrieve the matrix (set/get) 
## and create and retrieve the inverse (setInv/getInv)
## this object is used by cacheSolve to calculate and then cache the inverse of
## a matrix

makeCacheMatrix <- function(x = matrix()) {
    ## initialize the storage item for the cached matrix
    my_inv <- NULL
    
    ## set function to create matrix using super assignment to place in 
    ## higher level variable space
    set <- function(y) {
            x <<- y
            my_inv <<- NULL
    }
    ## get function retrieves the original matrix
    get <- function() x
    ## setInv places the Inverse(x) into the cached variable
    setInv <- function( mInv) my_inv <<- mInv
    ## getInv retrieves the 'cached' variable and returns it to calling spot
    getInv <- function() my_inv
    
    ## define the function names for the methods
    list(set = set, get = get, setInv = setInv, getInv = getInv)
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## check to see if inverse of 'x' has been calculated and return it
        ## if inverse not calculated, calculate & return inverse
    
        ## retrieve the calculated inverse 
        my_inv <- x$getInv()
    
        ## if my_inv has a value (i.e. it is NOT null), return my_inv
        if (!is.null(my_inv)) {
            message("getting cached inverse")
            return(my_inv)
        }
        
        ## if no cached inverse, we need to calculate it
        ## temp holder for matrix
        y <- x$get()
        ## use the solve command to invert the matrix
        my_inv <- solve(y)
        ## set the inverse into cached variables
        x$setInv(my_inv)
        ## return the inverse
        my_inv
}
