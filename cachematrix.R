## The following functions will be used to cache the inverse of 
## a matrix so that the inverse can be saved for future use 
## rather than it being recalculated for each use.

## The first function creates a list to store the matrix
## and it's inverse

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(t){
          x <<- t
          s <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) s<<- solve
        getinverse <- function() s
        list(set=set, get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}


## The second function checks to see if the inverse of the 
## matrix has already been calculated.  If so, it returns the 
## cached value.  If not, it calculates the inverse, saves it
## and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <-x$getinverse()
        if(!is.null(s)) {
          message("Getting cashed inverse")
          return(s)
        }
        cached <- x$get()
        s <- solve(cached, ...)
        x$setinverse(s)
        s
}
