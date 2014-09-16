## Create by Diego Marinho de Oliveira at 16th September, 2014

# This function defines a special vector that 
# can cache matrix inverse value
makeCacheMatrix <- function(x = matrix()) {
    
    inverse_ <- NULL
    
    set <- function(y) {
        x <<- y
        inverse_ <<- NULL
    }
    
    get <- function() x
    
    set_inverse <- function(inverse) inverse_ <<- inverse
    get_inverse <- function() inverse_
    list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}

## Return a matrix that is the inverse of 'x'
## Precondition: Matrix has inverse
## OBS: It's create a cache of the inversed value
cacheSolve <- function(x, ...) {
    
        inverse <- x$get_inverse()
        if(!is.null(inverse)) {
            message("getting cached data")
            return (inverse)
        }
        
        data <- x$get()
        inverse <- solve(data, ...)
        x$set_inverse(inverse)
        
        return (inverse)
}
