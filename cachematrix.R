## The two functions below work in unison to produce the inverse of a square invertible input matrix,
## and "cache" this result to be accessed again without re-calculation, if the input matrix is unchanged.

## The 1st function, makeCacheMatrix(), assigns values to variables and stores them in the parent environment, 
## then creates a list of functions to be drawn upon in the 2nd function, cacheSolve.
makeCacheMatrix <- function(x = matrix()){                          ##makeVector <- function(x = numeric()) {
        s <- NULL                                                           ##       m <- NULL
        set <- function(y){                                                 ##        set <- function(y) {
                x <<- y                                                     ##                x <<- y
                s <<- NULL                                                  ##               m <<- NULL
        }                                                                   ##        }
        get <- function() x                                                 ##        get <- function() x
        setinv <- function(solve) s <<- solve                    ##        setmean <- function(mean) m <<- mean
        getinv <- function() s                                              ##        getmean <- function() m
        list(set = set, get = get, setinv = setinv, getinv = getinv)        ##        list(set = set, get = get,
                                                                            ##             setmean = setmean,
                                                                            ##             getmean = getmean)
}                                                                           ##}
#########################################################################################################
#########################################################################################################

## The 2nd function, cacheSolve(), accesses the list of functions from makeCacheMatrix() and evaluates
## the value of the variable used to store the inverse of the input matrix to see if it has changed,
## then it returns the inverse of the input matrix, calculating it only if necessary.

cacheSolve <- function (x, ...){                                    ##cachemean <- function(x, ...) {
        s <- x$getinv()                                                     ##        m <- x$getmean()
        if(!is.null(s)) {                                                   ##        if(!is.null(m)) {
                message("getting cached data")                              ##                message("getting cached data")
                return(s)                                                   ##             return(m)
        }                                                                   ##        }
        data <- x$get()                                                     ##        data <- x$get()
        s <- solve(data, ...)                                               ##        m <- mean(data, ...)
        x$setinv(s)                                                         ##        x$setmean(m)
        s                                                                   ##        m
}                                                                           ##}

## Write a short comment describing this function

