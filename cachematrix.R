## This script is useful to demostrate how lexical scoping works 
## we use the <<- operator to set the value of x (the matrix) and inv (the inverted matrix)
## because we want to modify x and inv defined in the enclosing environment

## This function create the functions to save and read a matrix and 
## its inverse. The creation of a new matrix reset the relative inverse.
## The resulting functions are stored as a list of functions.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) inv <<- inverse
        getInv <- function() inv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}

## This function checks if an already cached inverted matrix exist
## if it exists, then return a message & the matrix itself
## if it doesn't, then retirve the original matrix, inverses it using
## solve() and stores the result inside the makeCacheMatrix 'object'

cacheSolve <- function(x) {
        inv <- x$getInv()
        if(!is.null(inv)) {
                message("getting cached inverted matrix")
                return(inv)
        }
        dat <- x$get()
        inv <- solve(dat)
        x$setInv(inv)
        return(inv)
}

## Create a new makeCacheMatrix 'object' using a 10x10 matrix
d <- makeCacheMatrix(matrix(rnorm(100), 10, 10))

## Run to store the inverted matrix in the cache
cacheSolve(d)

## Run twice to check if the result are really stored in cache
## (the message 'getting cached inverted matrix' should appear above the 
## inverted matrix)
cacheSolve(d)

## Change matrix
d <- makeCacheMatrix(matrix(rbinom(100, 1, 0.7), 10, 10))

## Inverted matrix should be recalculated
cacheSolve(d)
