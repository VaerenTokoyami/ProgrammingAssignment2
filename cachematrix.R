## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix
## Creates a cached(stored) version of the matrix 'x', while defining
## the inverse property as a toggle for when to return the matrix or
## the inverse of the matrix
## This is accomplished by establishing the matrix and saving it,
## then retrieving the Matrix, then establishing the inverse of the matrix
## , and finally retrieving the inverted matrix

makeCacheMatrix <- function(x, ...){
        i <- NULL
        set <- function(y){
                  # establishes the symbol "<<-" to assign an object's
                  # value to a different environment
                  x <<- y
                  i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<-inverse
        getinverse <- function () i
        list(set = set, 
             get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## CacheSolve
## Establishes the function which retrieves the mathematically
## inverse of a matrix that is returned by the function makeCacheMatrix (DEPENDANT)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                  message("getting cached data...")
                  return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
