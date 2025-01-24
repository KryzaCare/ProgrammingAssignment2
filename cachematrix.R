## Taking the course - R Programming in coursera includes assignments
## such as Lexical Scoping that is needed for students to accomplish.

## So, I'll be showing technical descriptions or comments that will explain how and what functions are used. 
## The functions were encoded below. 

## Since the functions involves repetitive computations, this normally takes a long time and requires more time
## BAsically, this fucntion will have a special matrix object for it to caches k

makeCacheMatrix <- function(x = matrix()) {			## Shows the default mode of "matrix" used to determine the point. 
k <- NULL							## k will serve as NULL and will keep the value of the matrix mean
set <- function(y) {						## When the new matrix is identified then you have to reset k to NULL and assign it to new
x <<- y
k <<- NULL
}
get <- function() x						## Defines the function for taking the frequency
setC <- function (mean) k <<- mean		        	## The value of mean will be assigned
getC <- function() k						## Shows the function to calculate the mean
list(set = set, get = get,				
setC = setC,
getC = getC)
}

## In this part, it already computes the mean of the special matrix that was stated above. 
## So, if the mean has already been computed but the matrix hasn't modified, the mean can
## most likely be retrieved from the cache.

cacheSolve <- function(x, ...) {				## Shows the computation from the above
k <- x$getmean()						## k will serve as the value of mean
if (!is.null(k)) {						## if the calculated mean in the matrix did not change
message ("getting cached data")					## then it will regain the mean 
return(k)
}
mtrx <- x$get()
k <- solve(mtrx, ...)
x$setC(k)
k
}
