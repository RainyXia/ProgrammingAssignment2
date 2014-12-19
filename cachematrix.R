## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        n <- NULL
        set <- function(y) {
                x <<- y
                n <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) n <<- solve
        getsolve <- function() n
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        n <- x$getsolve()
        if(!is.null(n)) {
                message("getting cached data")
                return(n)
        }
        data <- x$get()
        n <- solve(data, ...)
        x$setsolve(n)
        n
}
