## The overall code calculates the inverse of a matrix given by the user.
## The aim is to take some benefit to caching the inverse of a matrix rather
## than compute it repeatedly.
## In order to calculate the inverse of a matrix, follow these steps:
## 1. call function makeCacheMatrix with the argument of your matrix,
## 2. call function cacheSolve, and put the return value of the previous step
##    into its argument.

## Function makeCacheMatrix creates a special object that can cache the inverse.
## This function is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Function cacheSolve computes the inverse of the 'matrix' returned by function
## makeCacheMatrix. 
## This function first checks to see if the inverse matrix has already been
## calculated. If so, (and the matrix has not changed), then it gets the
## inverse matrix from the cache and skips computation. Otherwise, it calculates
## the invese of the matrix and sets the value of the inverse matrix in the
## cache via the setinverse function. 

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
