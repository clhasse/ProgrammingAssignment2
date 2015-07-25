## The functions first create a matrix and store the inverse of the matrix through 
## makeCacheMatrix and then, when the inverse of a matrix is asked for via cacheSolve, 
## the function (cacheSolve) looks to see if that inverse is stored and the stored  
## version is returned if it exists and if not, the inverse is calculated and returned.

## Specifically, makeCacheMatrix sets the value of the matrix, gets the value of the matrix, 
## sets the value of the inverse of the matrix, gets the value of the inverse matrix, and 
## caches the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) 
                m <<- inverse
        getInverse <- function() m
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) 
}

## cacheSolve identifies whether the inverse of the same matrix has already been computed 
## and if so, returns the applicable message and then the inverse from the cache; otherwise, 
## it computes the inverse of the matrix and returns that.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInverse(m)
        m
} 
## Please note (in the spirit of the honor code) that the assignment instructions 
## provided the majority of this code.