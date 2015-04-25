## These are R functions to create a matrix
## object and to cache its inverse
## To use these functions:
## Create a Matrix 
## 	  > M <- matrix(c(1,2,1,1), 2,2)
##	  > M
##	       [,1] [,2]
##  	  [1,]    1    1
##	  [2,]    2    1
## Next:
##	  > newM <- makeCacheMatrix()
##	  > newM$set(M)
## or:
##	  > newM <- makeCacheMatrix(M)
## Then:
##	  > cacheSolve(newM)
##	       [,1] [,2]
##	  [1,]   -1    1
##	  [2,]    2   -1



## Create a Cache Matrix

makeCacheMatrix <- function(x = matrix()) {

        m <-NULL

	  ## Feed in a Matrix data
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
	  ## Output the Matrix Data
        get <- function() x

        setInvMatrix <- function(inverse) m <<- inverse
        getInvMatrix <- function() m
	
	  ## The following line of code allows the $ index  
	  ## like: $set, $get, $setinv & $getinv to run the functions
	  ## set(), get(), setInvMatrix() and getInvMatrix() respectively
        list(set = set, get = get,
             setinv = setInvMatrix,
             getinv = getInvMatrix)
}


## cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), then 
## the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                ## return cached data
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
