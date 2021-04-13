## Doing the Matrix Inversion usually takes a long hussle and needs more time since 
## it requires a repeated computation 
## Here are the prepared pair of functions to be used that will 
## served as the special object that will basically store the matrix and 
## caches its inverse

## BAsically, this function will have a special matrix object for it to caches its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
                setInverse <- function (inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## In this part, this function computes already the inverse of the special matrix that was 
## mentioned and done above. 
## So, when the inverse is already calculated but then the matrix has not changed then 
##probably, the inverse should be retrieved from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message ("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
