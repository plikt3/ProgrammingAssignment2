## The makeCacheMatrix function does the following
## a) Sets the value of a matrix
## b) gets the value of a matrix
## c) Sets the value of the inverted matrix
## d) Gets the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## This cacheSolve function calculates the values of the inverted matrix
## once the it determines if the matrix has already been inverted or not.

cacheSolve <- function(x = matrix(), ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached matrix")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setmatrix(m)
        m
}
