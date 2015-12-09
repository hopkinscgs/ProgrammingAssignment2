## The first function, makeCacheMatrix creates a list to 
## store the information for the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL

#set the value of the matrix:
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

#get the value of x:
        get <- function() x

#set the value to inverse:
        setinverse <- function(inverse) m <<- inverse
        
#get the value of m:
        getinverse <- function() m
        
#the result contains four parts:
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}






## The second function, cacheSolve, takes the list created 
## by makeCacheMatrix and generate result of the inverse 
## matrix. It first checks to see if the inverse matrix has
## already been calculated. If so, it gets the inverse matrix
## from the cache and skip the computation. Otherwise, it
## calculates the inverse matrix of the data and sets the
## inverse matrix in the cache.

cacheSolve <- function(x, ...) {
        
#check if the inverse matrix is stored in the cache:
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

#if there is no inverse matrix in the cache, calculate from data.
        data <- x$get()
        m <- solve(data, ...)
        
#set the calculated inverse matrix to the cache
        x$setinverse(m)
        m

## Return a matrix that is the inverse of 'x'
}
