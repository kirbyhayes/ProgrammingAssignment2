## Taking the inverse of a matrix can be computationally expensive
## thus you may want to cache the inverse of a matrix rather than compute 
## that particular inverse multiple times.

## makeCacheMatrix is defining the "get" and set" condition in the environment so an
## inverse is taken if one does not exist in the cached environment.

makeCacheMatrix <- function(x = matrix()) {
                inv <- NULL
                set <- function(y) {
                        x <<- y
                        inv <<- NULL
                }
                #defining the "get" and "set" conditions in the environment
                get <- function() x
                setInverse <- function(inverse) inv <<- inverse
                getInverse <- function() inv
                list(set = set,
                     get = get,
                     setInverse = setInverse,
                     getInverse = getInverse)
        }
        

## cacheSolve looks for a cached inverse for 'x' before it calculates the inverse
## if a cached inverse exists cacheSolve will return the cached inverse value 
## while displaying the message 'cached inverse matrix found, getting the matrix'.

cacheSolve <- function(x, ...)  {
        inv <- x$getInverse()
        ## the "!" in front of is.null checks that inv is "not NULL" and the 'if' returns:
        ## 'cached inverse matrix found, getting the matrix' when inv is not NULL
        if (!is.null(inv)) {
                message("cached inverse matrix found, getting the matrix")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
