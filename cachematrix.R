## The function allows to store a matrix in an oject that provides getter
## and setter for the given matrix
## The function cashSolve returns the inverting of the given matrix. The
## inverting is calculated once. If the calculation already exists the cached
## matrix is returned

## makeCacheMatrix creates a list of functions available on the returned object
## available on the provided matrix x
## -set - allows to change the matrix of an already created object
## -get - returns the current matrix
## -setSolve - set/change the matrix provided by getSolve
## -getSolve - returns the current matrix set/changed by setSolve
##           - returns NULL in case it is not set

makeCacheMatrix <- function(x = matrix()) {
    ## set empty init of inverted matrix
    invMatrix <- NULL
    set <- function(y) {
        x <<- y
        ## clear invMatrix that it has to be recalculated
        invMatrix <<- NULL
    }
    get <- function() {
        x
    }
    setSolve <- function(invertedMatrix = matrix()) {
        invMatrix <<- invertedMatrix
    }
    getSolve <- function() {
        invMatrix
    }
    list(set = set, 
         get = get, 
         setSolve = setSolve, 
         getSolve = getSolve)
}


## cacheSolve returns an inverted matrix of the square matrix previously stored
## in the makeCacheMatrix-object
## additional parameters are assigned to the nested solve-function

cacheSolve <- function(x, ...) {
    ## call the getSolve-function on the given makeCacheMatrix-object
    invMatrix <- x$getSolve()
    ## if getSolve returns not Null the function cacheSolve will end with the
    ## returnvalue of getSolve
    if(!is.null(invMatrix)) {
        return(invMatrix)
    }
    ## if getSolve returns Null it has to be calculated
    ## temporary matrix-object data gets the matrix of x
    ## the inverting is calculated, stored to the makeCacheMatrix-object
    ## and is set as the returnvalue of the function
    data <- x$get()
    invMatrix <- solve(data, ...)
    x$setSolve(invMatrix)
    invMatrix
}

## Testing call
## source("c:/Users/Sonic/r/pa2/cachematrix.R")
## mx <- matrix(c(-1,-3,2,1,3,1,2,3,9), 3, 3)
## cmx <- makeCacheMatrix(mx)
## cs <- cacheSolve(cmx)
## cs
##      [,1]       [,2]      [,3]
## [1,]    0 -0.1111111 0.3333333
## [2,]   -1  0.5555556 0.3333333
## [3,]    1 -0.3333333 0.0000000
## Calculation check
## cmx$get() %*% cmx$getSolve()
## [,1]         [,2] [,3]
## [1,]    1 1.110223e-16    0
## [2,]    0 1.000000e+00    0
## [3,]    0 0.000000e+00    1