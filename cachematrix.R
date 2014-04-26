## The first function makeCacheMatrix  creates a list of four functions. It takes as an input a matrix object ,and the list functions
## inside the makeCacheMatrix can act upon the matrix object.for rg getInverse returns the inverse of the matrix.and get simply 
##returns the matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<-solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## cacheSolve takes in a makecacheMatrix object,and returns its innverse which is already cached .If the inverse of the matrix
## is not already cached ,it computes it and caches it ;it also returns the inverse in process

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
