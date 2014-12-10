## This function creates a special "matrix" object that can cache its inverse.
## set the value of the matrix
#get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        imx <- NULL
        set <- function(y) {
    	        x <<- y
                imx <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) imx <<- inverse
        getinverse <- function() imx
        list(get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated , then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        imx <- x$getinverse()
         if(!is.null(imx)){
                message("getting cached data")
                return(imx)
        }
        data <- x$get()
        imx <- solve(data)
        x$setinverse(imx) # assigns resulting inverse matrix to object x
	imx
}
