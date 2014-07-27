## The aim of the bellow functions compute the inverse of a Matrix
## to cache the result and to use the cache if it already exists.

## The Function makeCacheMatrix creates a special matrix object
## That can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL

        set <- function(y) {
                x <<- y
                m <<- NULL
        }

        get <- function() x

        setsolve <- function(solve) m <<- solve

        getsolve <- function() m

        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## The cacheSolve function will compute the inverse of a matrix.
## If the inverse has already been calculated it will retrieve the
## Inverse from the cache

cacheSolve <- function(x, ...) {
        ## Retrive result from cache
        m <- x$getsolve() 

	## the result is in cache
        if(!is.null(m)) { 
                return(m)
        }

	# The result is not in cache it solves the result
        data <- x$get() 
        m <- solve(data, ...) 
        x$setsolve(m)	
        m 
}
