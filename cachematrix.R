## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
