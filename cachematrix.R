## Creates a matrix that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(aMatrix) {
                x <<- aMatrix
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}

## Checks if inv already exists. If yes retrieve from cache and if no calculates inverse of matrix using solve
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        ## Return a matrix that is the inverse of 'x'
        x$setinverse(inv)
        inv
}