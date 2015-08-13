## This function creates an object cache matrix which contains
## the matrix x and its inverse I which is set to NULL at the initialization.
## It also associates the object with the methods:
## - set which initializes the cache matrix
## - get which returns the matrix
## - setinv which sets the value of the inverse of the matrix x
## - getinv which returns the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setinv <- function(inv) I <<- inv
        getinv <- function() I
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## This function computes the inverse of 'x' and keep it in the cache
## if it has not been computed yet. Then it returns the inverse.
## Otherwise, the inverse is already in the cache and the function 
## prints the message "getting cached data"

cacheSolve <- function(x, ...) {
        ## Getting the value stored in the cache
        I <- x$getinv()
        if(!is.null(I)) {
                ## If it is not empty, then the function returns the value stored
                message("getting cached data")
                return(I)
        }
        ## Else, it computes the inverse via the function solve
        data <- x$get()
        I <- solve(data, ...)
        ## And it sets the inverse in the object cache matrix   
        x$setinv(I)
        ## In the end, it returns the inverse matrix
        return(I)
}
