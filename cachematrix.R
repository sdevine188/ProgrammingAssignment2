## makeCacheMatrix function
## this function will create a list of functions that can be used to:
## 1) set the values of a matrix
## 2) get the values of a matrix
## 3) set the inverse of the matrix
## 4) get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#cachesolve function
## this function calculates the inverse of the matrix identified in the makeCacheMatrix function
## first, it checks to see if the inverse has already been calculated
## if so, it returns the cached inverse
## if not, it calculates and returns the inverse
cachesolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
