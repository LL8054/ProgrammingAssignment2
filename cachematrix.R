## Cachematrix.R allows for the creation of a matrix and the solving of its inverse.
## If there is a cached copy of the inverse it will return that instead of recomputing.
## This code is a slight tweak from the example functions "makeVector" and "cachemean" 
##      from the Week 2 Programming Assignment of Dr. Roger Peng's R 
##      Programming Course (rprof-007) in Coursera.
 

## makeCacheMatrix accepts the argument to create, store, and make available a matrix
## and also does the same for the inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## cacheSolve solves for the inverse of the matrix if there is none cached.  if there
## is an inverse cached then it returns that inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m
}
