## Put comments here that give an overall description of what your
## functions do

## Creating a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv = NULL
        set <- function(x) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        
        list(set = set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Returns the inverse of a matrix after first checking to see if it exists. If it does, it retuns the existing value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse() 
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
