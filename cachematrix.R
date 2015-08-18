## The two functions shown here help in caching the 
## inverse of a matrix to avoid recomputing it 
## again and again example in loops. 

## create a matrix which can be cached. 

makeCacheMatrix <- function(x = matrix()) {

	inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## to compute the inverse and cache the result. Calling
## cacheSolve again for same matrix will not compute the 
## inverse again, it will fetch it from the pre-computed
## result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv


}
