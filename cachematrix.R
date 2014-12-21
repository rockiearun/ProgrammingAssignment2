##This Program returns the inverse of a matrix by either calculating it or returning a cache of the function

## Function to return the inverse of the matrix given

makeCacheMatrix <- function(x = matrix()) {

        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)

}


## Generate a inverse or retrieve from the  cache

cacheSolve  <- function(x, ...) {
       
        i <- x$getinverse()
        if (!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
         ## Return a matrix that is the inverse of 'x'
        i
}
