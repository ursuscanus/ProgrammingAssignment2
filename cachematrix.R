## makeCacheMatrix and cacheSolve together build a framework to efficiently
## calculate the inverse of a matrix through establishing a cachd for it.
## As a result inverse will be only calculated if it has never been done
## before. That is always true when teh matrix has changed.

## makeCacheMatrix defines a vector/list of functions for setting and retrieving
## the matrix itseld as well as the inverse

makeCacheMatrix <- function(x = matrix()) {
        xinv <- NULL
        set <- function(y = matrix())   {
                x <<- y
                xinv <<- NULL
        }
        get <- function() x
        setInv <- function(yinv) xinv <<- yinv
        getInv <- function() xinv
        list(set = set,
             get = get,
             setInv = setInv,
             getInv = getInv)
}


## cacheSolve will return the inverse of the matrix argument either by 
## retreiving the previously calcuated and subsequently cached or by initiating
## a new calculation

cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        ## calculated before? if so return value
        if (!is.null(inv))   {
                message("retrieving cached inverse")
                return(inv)
        }
        ## retrieve original matrix, calcuate inverse and store in cache
        d <- x$get()
        inv <- solve(d, ...)
        x$setInv(inv)

        ## Return a matrix that is the inverse of 'x'
        inv
}
