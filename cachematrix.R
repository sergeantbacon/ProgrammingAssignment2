## These two functions minimize the cost of accessing the inverse of a matrix
##      repeatedly by leveraging caching. Operations that involve calculating
##      the inverse of a large matrix then accessing that inverse often should
##      see performance benefits by using the makeCacheMatrix function to
##      create the matrix and the cacheSolve function to get the inverse.

## makeCacheMatrix creates a matrix object with four functions:
##      set: sets the value of the matrix to the argument, 
##              initalizes the matrix inverse
##      get: returns the matrix
##      setinverse: sets the value of the matrix inverse to the argument
##      getinverse: returns the matrix inverse
##
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

## cacheSolve returns the matrix inverse of the cacheMatrix argument
##      if the inverse in the cacheMatrix is NULL, 
##              calculate the inverse,
##              return the calculated inverse, 
##              set the inverse in the cacheMatrix to the calculated inverse
##      if the cacheMatrix has the inverse set,
##              skip the calculation and return the existing inverse
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m          
}